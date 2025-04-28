{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- MyPandoc
-}

module GeneralParse where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit)

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f parser = Parser i
        where
            i str = case runParser parser str of
                Nothing -> Nothing
                Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser px) = Parser $ \input ->
        case pf input of
            Just (f, rest) -> case px rest of
                Just (x, rest') -> Just (f x, rest')
                Nothing -> Nothing
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser (const Nothing)
    p1 <|> p2 = Parser $ \s -> case runParser p1 s of
        Nothing -> runParser p2 s
        Just (a, rest) -> Just (a, rest)

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input ->
        case p input of
            Just (result, rest) -> runParser (f result) rest
            Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | x == c = Just (c, xs)
      | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar s = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | x `elem` s = Just (x, xs)
      | otherwise = Nothing

parseUInt :: Parser Word
parseUInt = read <$> some parseDigit

parseDigit :: Parser Char
parseDigit = Parser f
  where
    f (x:xs)
      | isDigit x = Just (x, xs)
      | otherwise = Nothing
    f [] = Nothing

parseInt :: Parser Int
parseInt = do
    sign <- (parseChar '-' >> return "-") <|> return ""
    digits <- some parseDigit
    return (read (sign ++ digits))

parseString :: String -> Parser String
parseString = traverse parseChar

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

parseWhitespace :: Parser ()
parseWhitespace = () <$ many (parseAnyChar " \t\n\r")

consumeWhitespaces :: Parser a -> Parser a
consumeWhitespaces p = parseWhitespace *> p <* parseWhitespace

symbol :: Char -> Parser Char
symbol c = consumeWhitespaces (parseChar c)

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = do
    symbol '('
    a <- consumeWhitespaces p
    symbol ','
    b <- consumeWhitespaces p
    symbol ')'
    return (a, b)

parseTruple :: Parser a -> Parser (a, a, a)
parseTruple p = do
    symbol '('
    a <- consumeWhitespaces p
    symbol ','
    b <- consumeWhitespaces p
    symbol ','
    c <- consumeWhitespaces p
    symbol ')'
    return (a, b, c)
