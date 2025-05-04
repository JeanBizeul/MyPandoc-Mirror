{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- JsonParse
-}

module JsonParse (
    JsonValue(..),
    parseWhiteSpace,
    parseNull,
    parseBool,
    parseNumber,
    parseJsonString,
    parseJsonArray,
    parseJsonObject,
    parseKeyValue,
    parseJsonKey,
    parseJsonValue,
    jsonToDocument
) where

import GeneralParse
import Document
import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)
import Control.Applicative ((<|>), many, some, empty)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Double
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

parseWhiteSpace :: Parser ()
parseWhiteSpace = Parser $ \input ->
    let rest = dropWhile isSpace input
    in Just ((), rest)

parseNull :: Parser JsonValue
parseNull = Parser $ \input ->
    case stripPrefix "null" input of
        Just rest -> Just (JsonNull, rest)
        _ -> Nothing

parseBool :: Parser JsonValue
parseBool = Parser $ \input ->
    case stripPrefix "true" input of
        Just rest -> Just (JsonBool True, rest)
        Nothing -> case stripPrefix "false" input of
            Just rest' -> Just (JsonBool False, rest')
            Nothing -> Nothing

parseNumber :: Parser JsonValue
parseNumber = do
    sign <- parseSign
    intPart <- parseIntegerPart
    fracPart <- parseFractionPart
    let numStr = sign ++ intPart ++ fracPart
    parseNumberFromString numStr

parseSign :: Parser String
parseSign = (parseChar '-' >> return "-") <|> return ""

parseIntegerPart :: Parser String
parseIntegerPart = some parseDigit

parseFractionPart :: Parser String
parseFractionPart = (do
    dot <- parseChar '.'
    decimals <- some parseDigit
    return (dot : decimals)) <|> return ""

parseNumberFromString :: String -> Parser JsonValue
parseNumberFromString numStr =
    case reads numStr :: [(Double, String)] of
        [(num, "")] -> return (JsonNumber num)
        _           -> empty

parseJsonString :: Parser JsonValue
parseJsonString = do
    _ <- parseChar '"'
    content <- many parseStringChar
    _ <- parseChar '"'
    return (JsonString content)

parseStringChar :: Parser Char
parseStringChar = parseEscape <|> parseNormalChar

parseEscape :: Parser Char
parseEscape = parseChar '\\' *> (parseSimpleEscape <|> parseDefaultEscape)

parseSimpleEscape :: Parser Char
parseSimpleEscape = parseAnyChar "\"\\/bfnrt" >>= mapEscape

mapEscape :: Char -> Parser Char
mapEscape escaped = return $ case escaped of
    '"'  -> '"'
    '\\' -> '\\'
    '/'  -> '/'
    'b'  -> '\b'
    'f'  -> '\f'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    _    -> escaped

parseDefaultEscape :: Parser Char
parseDefaultEscape = empty

parseNormalChar :: Parser Char
parseNormalChar = Parser $ \input ->
    case input of
        []      -> Nothing
        ('"':_) -> Nothing
        (c:cs)  -> Just (c, cs)

parseJsonArray :: Parser JsonValue
parseJsonArray = do
    _ <- parseChar '[' *> parseWhiteSpace
    elements <- parseJsonValue
        `sepBy` (parseWhiteSpace *> parseChar ',' <* parseWhiteSpace)
    _ <- parseWhiteSpace *> parseChar ']'
    return (JsonArray elements)

parseJsonObject :: Parser JsonValue
parseJsonObject = do
    _ <- parseChar '{' *> parseWhiteSpace
    pairs <- parseKeyValue
        `sepBy` (parseWhiteSpace *> parseChar ',' <* parseWhiteSpace)
    _ <- parseWhiteSpace *> parseChar '}'
    return (JsonObject pairs)

parseKeyValue :: Parser (String, JsonValue)
parseKeyValue = do
    key <- parseJsonKey
    _ <- parseWhiteSpace *> parseChar ':' <* parseWhiteSpace
    value <- parseJsonValue
    return (key, value)

parseJsonKey :: Parser String
parseJsonKey = do
    result <- parseJsonString
    case result of
        JsonString key -> return key
        _ -> empty

parseJsonValue :: Parser JsonValue
parseJsonValue = parseWhiteSpace *>
    (parseNull <|> parseBool <|> parseNumber <|> parseJsonString
        <|> parseJsonArray <|> parseJsonObject) <* parseWhiteSpace

jsonToDocument :: JsonValue -> Maybe Document
jsonToDocument (JsonObject obj) = do
    let objMap = M.fromList obj
    headerVal <- M.lookup "header" objMap
    bodyVal <- M.lookup "body" objMap
    header <- jsonToHeader headerVal
    body <- jsonToContentList bodyVal
    return $ Document header body
jsonToDocument _ = Nothing

jsonToHeader :: JsonValue -> Maybe Header
jsonToHeader (JsonObject obj) = do
    let objMap = M.fromList obj
    JsonString title <- M.lookup "title" objMap
    let author = case M.lookup "author" objMap of
            Just (JsonString a) -> Just a
            _                   -> Nothing
    let date = case M.lookup "date" objMap of
            Just (JsonString d) -> Just (Date d)
            _                   -> Nothing
    return $ Header title author date
jsonToHeader _ = Nothing

jsonToContentList :: JsonValue -> Maybe [Content]
jsonToContentList (JsonArray values) = mapM jsonToContent values
jsonToContentList _ = Nothing

jsonToContent :: JsonValue -> Maybe Content
jsonToContent (JsonObject obj) =
    let objMap = M.fromList obj in
    case M.lookup "type" objMap of
        Just (JsonString "text") ->
            case M.lookup "content" objMap of
                Just (JsonString str) -> Just $ Text str
                _ -> Nothing
        _ -> Nothing
jsonToContent (JsonString str) = Just (Text str)
jsonToContent _ = Nothing
