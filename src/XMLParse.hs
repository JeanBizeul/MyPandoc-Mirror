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
    parseSimpleBalise,
    parseDoubleBalise,
    parseXML
) where

import GeneralParse
import Data.Char
import Data.List
import Data.Maybe
import Data.Bool
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
      let (title, rest) = span (\c -> c /= '>' && c /= ' ' && c /= '<') input
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

parseRestCorrect :: Parser String
parseRestCorrect = Parser f
  where
    f [] = Just ("", "")
    f input =
      let (argcontent, rest) = span (\c -> c /= '\0') input
      in if null argcontent
         then Nothing
         else Just (argcontent, rest)

parseRestIncorrect :: Parser String
parseRestIncorrect = Parser f
  where
    f [] = Just ("", "")
    f input =
      let (argcontent, rest) = span (\c -> c /= '\0') input
      in if null argcontent
         then Nothing
         else Nothing

parseBalise :: Parser Balise
parseBalise = do
    symbol '<'
    title <- parseTitle
    argtag <- consumeWhitespaces parseArgTag
    argcontent <- consumeXMLWhitespaces parseArgContent
    symbol '>'
    rest <- parseRestCorrect
    return (Balise title (Just [(BaliseArg argtag (Just argcontent))]))

parseSimpleBalise :: Parser Balise
parseSimpleBalise = do
    symbol '<'
    title <- parseTitle
    symbol '>'
    parseRestIncorrect
    return (Balise title Nothing)

parseContentBetween :: String -> Parser String
parseContentBetween stopStr = Parser f
  where
    f input = go "" input
      where
        go acc rest
          | stopStr `isPrefixOf` rest = Just (reverse acc, rest)
          | null rest = Nothing
          | otherwise =
              let (c:cs) = rest
              in go (c:acc) cs

getTitleBalise :: Parser Balise
getTitleBalise = do
    symbol '<'
    title <- parseTitle
    symbol '>'
    return (Balise title Nothing)

stringToBaliseArg :: String -> BaliseArg
stringToBaliseArg str =
  case runParser parseDoubleBalise str of
    Just (Balise title (Just [BaliseArg _ content]), _) ->
      BaliseArg title content
    _ -> BaliseArg "error" Nothing

parseBaliseArgs :: String -> String -> [BaliseArg]
parseBaliseArgs parentTag input = go input []
  where
    go "" acc = reverse acc
    go rest acc =
      case runParser parseDoubleBaliseTwo rest of
        Just (Balise tag (Just args), remaining) ->
          go (dropWhile isSpace remaining) (acc ++ args)
        Just (_, remaining) ->
          go (dropWhile isSpace remaining) acc
        Nothing -> reverse acc

parseDoubleBaliseTwo :: Parser Balise
parseDoubleBaliseTwo = do
    title <- getTitleBalise
    content <- parseContentBetween ("<" ++ "/" ++ (baliseTitle title) ++ ">")
    getTitleBalise
    return (Balise (baliseTitle title)
      (Just [(BaliseArg (baliseTitle title) (Just content))]))

parseDoubleBalise :: Parser Balise
parseDoubleBalise = do
    title <- getTitleBalise
    let closing = "</" ++ baliseTitle title ++ ">"
    content <- parseContentBetween closing
    _ <- parseString closing
    let innerArgs = parseBaliseArgs (baliseTitle title) content
    let finalArgs = if null innerArgs
                    then [BaliseArg (baliseTitle title) (Just content)]
                    else innerArgs
    return (Balise (baliseTitle title) (Just finalArgs))

parseOr:: Parser a -> Parser a -> Parser a
parseOr (Parser p1) (Parser p2) = Parser $ \input ->
    case p1 input of
        Just result -> Just result
        Nothing     -> p2 input

parseXML :: Parser Balise
parseXML = parseDoubleBalise `parseOr` parseBalise `parseOr` parseSimpleBalise

--Order of magnitude
--
--DoubleBalise : fail at simple balise and balise with args
--
--Balise : fail at simple balise but success at double balise
--
--SimpleBalise : Always successful
