{-
-- EPITECH PROJECT, 2025
-- mirror
-- File description:
-- MarkdownWrite
-}

module MarkdownWrite (writeMarkdown) where

import Document

writeMarkdown :: Document -> Maybe String
writeMarkdown (Document hdr body)
    | null (title hdr) = Nothing
    | otherwise = Just $ unlines $
        [ "---"
        , "title: " ++ title hdr
        ] ++ maybe [] (\a -> ["author: " ++ a]) (author hdr) ++
        maybe [] (\(Date d) -> ["date: " ++ d]) (date hdr) ++
        [ "---", "" ] ++ concatMap writeContent body

authorLineFromHeader :: Header -> [String]
authorLineFromHeader hdr = maybe [] (\a -> ["author: " ++ a]) (author hdr)

dateLineFromHeader :: Header -> [String]
dateLineFromHeader hdr = maybe [] (\(Date d) -> ["date: " ++ d]) (date hdr)

writeContent :: Content -> [String]
writeContent (Text s) = [s]
writeContent (Formatted fmt inner) = [formatWrap fmt (writeInline inner)]
writeContent (LinkContent (Link lbl url)) =
    ["[" ++ concatMap writeInline lbl ++ "](" ++ url ++ ")"]
writeContent (ImageContent (Image alt url)) =
    ["![" ++ alt ++ "](" ++ url ++ ")"]
writeContent (ParagraphContent (Paragraph cs)) =
    [concatMap writeInline cs, ""]
writeContent (SectionContent (Section mTitle cs)) =
    maybe [] writeTitle mTitle ++ concatMap writeContent cs
writeContent (CodeBlockContent (CodeBlock code _)) =
    ["```"] ++ [concatMap writeInline code] ++ ["```", ""]
writeContent (ListContent (List items)) =
    concatMap writeItem items ++ [""]
writeContent (TitleContent (Title t l)) =
    [replicate l '#' ++ " " ++ t, ""]

writeInline :: Content -> String
writeInline (Text s) = s
writeInline (Formatted fmt inner) = formatWrap fmt (writeInline inner)
writeInline (LinkContent (Link lbl url)) = 
    "[" ++ concatMap writeInline lbl ++ "](" ++ url ++ ")"
writeInline (ImageContent (Image alt url)) = 
    "![" ++ alt ++ "](" ++ url ++ ")"
writeInline _ = ""

formatWrap :: FormatType -> String -> String
formatWrap (Italic _) s = "*" ++ s ++ "*"
formatWrap (Bold _) s   = "**" ++ s ++ "**"
formatWrap (Code _) s   = "`" ++ s ++ "`"

writeItem :: Item -> [String]
writeItem (Item cs) = ["- " ++ concatMap writeInline cs]

writeTitle :: Title -> [String]
writeTitle (Title t l) = [replicate l '#' ++ " " ++ t, ""]
