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
import Debug.Trace

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

parseRest :: Parser String
parseRest = Parser f
  where
    f [] = Just ("", "")
    f input =
      let (argcontent, rest) = span (\c -> c /= '\0') input
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
    rest <- parseRest
    return (trace (show rest) (Balise title
        (Just [(BaliseArg argtag (Just argcontent))])))

parseSimpleBalise :: Parser Balise
parseSimpleBalise = do
    symbol '<'
    title <- parseTitle
    symbol '>'
    return (Balise title Nothing)

parseContentBetween :: Parser String
parseContentBetween = Parser f
  where
    f [] = Nothing
    f input =
      let (content, rest) = span (\c -> c /= '<') input
      in if null content
         then Nothing
         else Just (content, rest)

parseDoubleBalise :: Parser Balise
parseDoubleBalise = do
    title <- parseSimpleBalise
    content <- parseContentBetween
    parseSimpleBalise
    return(Balise (baliseTitle title) (Just [(BaliseArg "content" (Just content))]))

--  <document>
--  <header title=\"Simple example\"></header>
--  <body>
--  <paragraph>This is a simple example</paragraph>
--  </body>
--  </document>


--Order of magnitude
--
--DoubleBalise : fail at simple balise and balise with args
--
--Balise : fail at simple balise but success at double balise
--
--SimpleBalise : Always successful
