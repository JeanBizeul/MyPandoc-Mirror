{-
-- EPITECH PROJECT, 2025
-- mirror
-- File description:
-- MarkdownParse
-}

module MarkdownParse (parseMarkdown) where

import Document
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.List (stripPrefix, isPrefixOf, isSuffixOf, isInfixOf, tails)
import Data.Char (isSpace)

breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn delim str = go str
  where
    go [] = ([], [])
    go s@(x:xs)
      | delim `isPrefixOf` s = ([], drop (length delim) s)
      | otherwise = let (first, rest) = go xs in (x:first, rest)

parseMarkdown :: String -> Maybe Document
parseMarkdown input =
    let ls = lines input
        (headerLines, contentLines) = splitHeader ls
        hdr = parseHeader headerLines
        body = parseBody contentLines
    in if isValidHeader hdr
        then Just (Document hdr body)
        else Nothing

splitHeader :: [String] -> ([String], [String])
splitHeader ("---":xs) =
    let (hdr, rest) = break (== "---") xs
    in (hdr, drop 1 rest)
splitHeader xs = ([], xs)

parseHeader :: [String] -> Header
parseHeader ls = Header
    (fromMaybe "" $ lookup "title" kvs)
    (lookup "author" kvs)
    (Date <$> lookup "date" kvs)
  where
    kvs = mapMaybe parseKV ls

parseKV :: String -> Maybe (String, String)
parseKV line =
    case span (/= ':') line of
        (key, ':':val) -> Just (trim key, trim val)
        _              -> Nothing

isValidHeader :: Header -> Bool
isValidHeader (Header t _ _) = not (null t)

parseBody :: [String] -> [Content]
parseBody [] = []
parseBody (l:ls)
    | all (== '-') l || null l = parseBody ls
    | take 3 l == "```" = parseCodeBlock ls
    | take 2 l == "##" = parseSection l ls
    | take 2 l == "- " = parseList l ls
    | otherwise = parseParagraph l ls

parseCodeBlock :: [String] -> [Content]
parseCodeBlock ls =
    let (code, rest) = break (== "```") ls
    in CodeBlockContent 
    (CodeBlock (map Text code) Nothing) : parseBody (drop 1 rest)

parseSection :: String -> [String] -> [Content]
parseSection l ls =
    let t = trim $ drop 2 l
        title = Title t 2
        section = Section (Just title) []
    in SectionContent section : parseBody ls

parseList :: String -> [String] -> [Content]
parseList l ls =
    let (items, rest) = span (\x -> take 2 x == "- ") (l:ls)
    in ListContent (List (map parseItem items)) : parseBody rest

parseParagraph :: String -> [String] -> [Content]
parseParagraph l ls = 
    ParagraphContent (Paragraph [parseInlines l]) : parseBody ls

parseItem :: String -> Item
parseItem l = Item [parseInlines (drop 2 l)]

parseInlines :: String -> Content
parseInlines s
    | "**" `isWrappedIn` s = 
        Formatted (Bold [Text $ unwrap "**" s]) (Text $ unwrap "**" s)
    | "*"  `isWrappedIn` s = 
        Formatted (Italic [Text $ unwrap "*" s]) (Text $ unwrap "*" s)
    | "`"  `isWrappedIn` s = 
        Formatted (Code (unwrap "`" s)) (Text $ unwrap "`" s)
    | "[" `isInfixOf` s && "](" `isInfixOf` s = parseLink s
    | "![" `isInfixOf` s && "](" `isInfixOf` s = parseImage s
    | otherwise = Text s

parseLink :: String -> Content
parseLink s =
    case span (/= ']') (drop 1 s) of
        (lbl, ']':'(':url) -> LinkContent (Link [Text lbl] (init url))
        _ -> Text s

parseImage :: String -> Content
parseImage s =
    case span (/= ']') (drop 2 s) of
        (alt, ']':'(':url) -> ImageContent (Image alt (init url))
        _ -> Text s

isWrappedIn :: String -> String -> Bool
isWrappedIn delim s = delim `isPrefixOf` s && delim `isSuffixOf` s

unwrap :: String -> String -> String
unwrap d s = fromMaybe s (stripPrefix d =<< stripSuffix d s)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf xs = reverse <$> stripPrefix (reverse suf) (reverse xs)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
