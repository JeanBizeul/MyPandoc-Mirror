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
) where

import GeneralParse
import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)
import Control.Applicative ((<|>), many, some, empty)

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
