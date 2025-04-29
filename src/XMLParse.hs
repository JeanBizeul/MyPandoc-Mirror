{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- XMLParse
-}

module XMLParse (
    BaliseArg(..),
    Balise(..),
    parseXMLWhitespaces,
    consumeXMLWhitespaces,
    parseTitle,
    parseArgTag,
    parseArgContent,
    parseBalise,
    parseSimpleBalise
) where

import GeneralParse
import Data.Char
import Data.List
import Control.Applicative (many)


data BaliseArg = BaliseArg {
    baliseArgTag :: String,
    baliseArgContent :: Maybe String
}deriving (Show, Eq)

data Balise = Balise {
    baliseTitle :: String,
    baliseArgs :: Maybe [BaliseArg]
}deriving (Show, Eq)

parseXMLWhitespaces :: Parser ()
parseXMLWhitespaces = () <$ many (parseAnyChar " \t\n\r=\"")

consumeXMLWhitespaces :: Parser a -> Parser a
consumeXMLWhitespaces p = parseXMLWhitespaces *> p <* parseXMLWhitespaces

parseTitle :: Parser String
parseTitle = Parser f
  where
    f [] = Nothing
    f input =
      let (title, rest) = span (\c -> c /= '>' && c /= ' ') input
      in if null title
         then Nothing
         else Just (title, rest)

parseArgTag :: Parser String
parseArgTag = Parser f
  where
    f [] = Nothing
    f input =
      let (argtag, rest) = span (\c -> c /= '=' && c /= ' ') input
      in if null argtag
         then Nothing
         else Just (argtag, rest)

parseArgContent :: Parser String
parseArgContent = Parser f
  where
    f [] = Nothing
    f input =
      let (argcontent, rest) = span (\c -> c /= '>' && c /= '\"') input
      in if null argcontent
         then Nothing
         else Just (argcontent, rest)

parseBalise :: Parser Balise
parseBalise = do
    symbol '<'
    title <- parseTitle
    argtag <- consumeWhitespaces parseArgTag
    argcontent <- consumeXMLWhitespaces parseArgContent
    symbol '>'
    return (Balise title (Just [(BaliseArg argtag (Just argcontent))]))

parseSimpleBalise :: Parser Balise
parseSimpleBalise = do
    symbol '<'
    title <- parseTitle
    symbolRev '>'
    return (Balise title Nothing)

--  <document>
--  <header title="Simple example"></header>
--  <body>
--  <paragraph>This is a simple example</paragraph>
--  </body>
--  </document>
