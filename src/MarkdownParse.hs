{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- MarkdownParser
-}

module MarkdownParse (parseMarkdown) where

import Document
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (stripPrefix, isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace)

parseMarkdown :: String -> Document
parseMarkdown input =
    let ls = lines input
        (headerLines, contentLines) = splitHeader ls
        hdr = parseHeader headerLines
        body = parseBody contentLines
    in Document hdr body

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

parseBody :: [String] -> [Content]
parseBody [] = []
parseBody (l:ls)
    | all (== '-') l || null l = parseBody ls
    | take 3 l == "```" =
        let (code, rest) = break (== "```") ls
        in CodeBlockContent (CodeBlock (unlines code) Nothing) : parseBody (drop 1 rest)
    | take 2 l == "##" =
        let t = trim $ drop 2 l
        in SectionContent (Section (Just t) []) : parseBody ls
    | take 2 l == "- " =
        let (items, rest) = span (\l -> take 2 l == "- ") (l:ls)
        in ListContent (List (map parseItem items)) : parseBody rest
    | otherwise =
        ParagraphContent (Paragraph [parseInlines l]) : parseBody ls

parseItem :: String -> Item
parseItem l = Item [parseInlines (drop 2 l)]

parseInlines :: String -> Content
parseInlines s
    | "**" `isWrappedIn` s = Formatted Bold (Text $ unwrap "**" s)
    | "*"  `isWrappedIn` s = Formatted Italic (Text $ unwrap "*" s)
    | "`"  `isWrappedIn` s = Formatted Code (Text $ unwrap "`" s)
    | "[" `isInfixOf` s && "](" `isInfixOf` s = parseLink s
    | "![" `isInfixOf` s && "](" `isInfixOf` s = parseImage s
    | otherwise = Text s

isWrappedIn :: String -> String -> Bool
isWrappedIn delim s = delim `isPrefixOf` s && delim `isSuffixOf` s

unwrap :: String -> String -> String
unwrap d s = fromMaybe s (stripPrefix d =<< stripSuffix d s)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf xs = reverse <$> stripPrefix (reverse suf) (reverse xs)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseLink :: String -> Content
parseLink s =
    case span (/= ']') (drop 1 s) of
        (label, ']':'(':url) -> LinkContent (Link label (init url))
        _                    -> Text s

parseImage :: String -> Content
parseImage s =
    case span (/= ']') (drop 2 s) of
        (alt, ']':'(':url) -> ImageContent (Image alt (init url))
        _                  -> Text s
