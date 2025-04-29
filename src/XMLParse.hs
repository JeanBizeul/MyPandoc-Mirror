{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- XMLParse
-}

module XMLParse (
    BaliseArg(..),
    Balise(..),
    parseMore,
    consumeMore,
    parseTitle,
    parseArgTag,
    parseArgContent,
    parseBalise
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

parseMore :: Parser ()
parseMore = () <$ many (parseAnyChar " \t\n\r=\"")

consumeMore :: Parser a -> Parser a
consumeMore p = parseMore *> p <* parseMore

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
      let (argcontent, rest) = span (\c -> c /= '>') input
      in if null argcontent
         then Nothing
         else Just (argcontent, rest)

parseBalise :: Parser Balise
parseBalise = do
    symbol '<'
    title <- parseTitle
    argtag <- consumeWhitespaces parseArgTag
    argcontent <- consumeMore parseArgContent
    symbolRev '>'
    return (Balise title (Just [(BaliseArg argtag (Just argcontent))]))