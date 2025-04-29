{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- XMLParse
-}

import GeneralParse
import Data.Char
import Data.List

data BaliseArg = BaliseArg {
    baliseArgTag :: String,
    baliseArgContent :: Maybe String
}deriving (Show, Eq)

data Balise = Balise {
    baliseTitle :: String,
    baliseArgs :: Maybe [BaliseArg]
}deriving (Show, Eq)

parseTitle :: Parser String
parseTitle = Parser f
  where
    f [] = Nothing
    f input =
      let (title, rest) = span (\c -> c /= '>' && c /= ' ') input
      in if null title
         then Nothing
         else Just (title, rest)

parseBalise :: Parser Balise
parseBalise = do
    symbol '<'
    title <- parseTitle
    symbolRev '>'
    return (Balise title Nothing)