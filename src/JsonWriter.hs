{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- JsonWriter
-}

module JsonWriter (
    documentToJson
) where

import Document
import Data.List (intercalate)

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = [c]

jsonField :: String -> String -> String
jsonField key val = "\"" ++ key ++ "\":" ++ val

maybeField :: String -> Maybe String -> [String]
maybeField _ Nothing = []
maybeField key (Just val) = [jsonField key val]

jsonArray :: [String] -> String
jsonArray elems = "[" ++ intercalate "," elems ++ "]"

jsonString :: String -> String
jsonString s = "\"" ++ escapeString s ++ "\""

documentToJson :: Document -> String
documentToJson doc = 
    "{" ++ intercalate "," [
        jsonField "header" (headerToJson (header doc)),
        jsonField "body" (jsonArray (map contentToJson (body doc)))
    ] ++ "}"

headerToJson :: Header -> String
headerToJson (Header title author date) =
    "{" ++ intercalate "," (
        [jsonField "title" (jsonString title)] ++
        maybeField "author" (jsonString <$> author) ++
        maybeField "date" (dateToJson date)
    ) ++ "}"

dateToJson :: Maybe Date -> Maybe String
dateToJson = fmap (\(Date d) -> jsonString d)

titleToJson :: Title -> String
titleToJson (Title text level) =
    "{" ++ intercalate "," [
        jsonField "text" (jsonString text),
        jsonField "level" (show level)
    ] ++ "}"

contentToJson :: Content -> String
contentToJson (Text str) = textToJson str
contentToJson (TitleContent t) = titleContentToJson t
contentToJson (Formatted f c) = formattedToJson f c
contentToJson (LinkContent l) = linkToJson l
contentToJson (ImageContent i) = imageToJson i
contentToJson (ParagraphContent p) = paragraphToJson p
contentToJson (SectionContent s) = sectionToJson s
contentToJson (CodeBlockContent cb) = codeBlockToJson cb
contentToJson (ListContent l) = listToJson l

textToJson :: String -> String
textToJson str = "{" ++ intercalate "," [
    jsonField "type" (jsonString "text"),
    jsonField "content" (jsonString str)
    ] ++ "}"

titleContentToJson :: Title -> String
titleContentToJson t = "{" ++ intercalate "," [
    jsonField "type" "\"Title\"",
    jsonField "text" (jsonString (titleText t)),
    jsonField "level" (show (titleLevel t))
    ] ++ "}"

formattedToJson :: FormatType -> Content -> String
formattedToJson f c = case f of
    Italic cs -> formatCase "Italic" "italicContent" cs c
    Bold cs   -> formatCase "Bold" "boldContent" cs c
    Code str  -> "{" ++ intercalate "," [
        jsonField "type" "\"Formatted\"",
        jsonField "format" (jsonString "Code"),
        jsonField "codeContent" (jsonString str),
        jsonField "content" (contentToJson c)
        ] ++ "}"

formatCase :: String -> String -> [Content] -> Content -> String
formatCase fType field cs c = 
    "{" ++ intercalate "," [
        jsonField "type" "\"Formatted\"",
        jsonField "format" (jsonString fType),
        jsonField field (jsonArray (map contentToJson cs)),
        jsonField "content" (contentToJson c)
    ] ++ "}"

linkToJson :: Link -> String
linkToJson (Link label linkUrl) = 
    "{" ++ intercalate "," [
        jsonField "type" "\"Link\"",
        jsonField "label" (jsonArray (map contentToJson label)),
        jsonField "url" (jsonString linkUrl)
    ] ++ "}"

imageToJson :: Image -> String
imageToJson (Image alt url) = 
    "{" ++ intercalate "," [
        jsonField "type" "\"Image\"",
        jsonField "altText" (jsonString alt),
        jsonField "url" (jsonString url)
    ] ++ "}"

paragraphToJson :: Paragraph -> String
paragraphToJson (Paragraph cs) = 
    "{" ++ intercalate "," [
        jsonField "type" "\"Paragraph\"",
        jsonField "content" (jsonArray (map contentToJson cs))
    ] ++ "}"

sectionToJson :: Section -> String
sectionToJson (Section mt cs) = 
    "{" ++ intercalate "," (
        [jsonField "type" "\"Section\""]
        ++ maybeField "title" (titleToJson <$> mt)
        ++ [jsonField "content" (jsonArray (map contentToJson cs))]
    ) ++ "}"

codeBlockToJson :: CodeBlock -> String
codeBlockToJson (CodeBlock cs lang) = 
    "{" ++ intercalate "," (
        [jsonField "type" "\"CodeBlock\"",
         jsonField "code" (jsonArray (map contentToJson cs))]
        ++ maybeField "language" (jsonString <$> lang)
    ) ++ "}"

listToJson :: List -> String
listToJson (List items) = 
    "{" ++ intercalate "," [
        jsonField "type" "\"List\"",
        jsonField "items" (jsonArray (map itemToJson items))
    ] ++ "}"

itemToJson :: Item -> String
itemToJson (Item cs) = jsonArray (map contentToJson cs)