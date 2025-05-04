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
    parseXMLbalise,
    parseXMLDocument
) where

import GeneralParse
import Document
import Data.Char
import Data.List
import Data.Maybe
import Data.Bool
import Control.Applicative (some, many, empty, (<|>), optional, Alternative(..))
import Debug.Trace
import Control.Monad

data BaliseArg = BaliseArg {
    bT :: String,
    bC :: Maybe String
}deriving (Show, Eq)

data Balise = Balise {
    balTit :: String,
    balArg :: Maybe [BaliseArg]
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
    content <- parseContentBetween ("<" ++ "/" ++ (balTit title) ++ ">")
    getTitleBalise
    return (Balise (balTit title)
      (Just [(BaliseArg (balTit title) (Just content))]))

parseDoubleBalise :: Parser Balise
parseDoubleBalise = do
    title <- getTitleBalise
    let closing = "</" ++ balTit title ++ ">"
    content <- parseContentBetween closing
    _ <- parseString closing
    let innerArgs = parseBaliseArgs (balTit title) content
    let finalArgs = if null innerArgs
                    then [BaliseArg (balTit title) (Just content)]
                    else innerArgs
    return (Balise (balTit title) (Just finalArgs))

paOr:: Parser a -> Parser a -> Parser a
paOr (Parser p1) (Parser p2) = Parser $ \input ->
    case p1 input of
        Just result -> Just result
        Nothing     -> p2 input

parseXMLbalise :: Parser Balise
parseXMLbalise = parseDoubleBalise `paOr` parseBalise `paOr` parseSimpleBalise

parseQuotedString :: Parser String
parseQuotedString = do
  symbol '"'
  content <- many (Parser f)
  symbol '"'
  return content
  where
    f [] = Nothing
    f ('"':_) = Nothing
    f (x:xs)  = Just (x, xs)

parseAttribute :: Parser (String, String)
parseAttribute = do
  consumeWhitespaces (return ())
  key <- parseArgTag
  symbol '='
  value <- parseQuotedString
  return (key, value)

parseHeaderStart :: Parser (String, [(String, String)])
parseHeaderStart = do
    symbol '<'
    title <- parseTitle
    attrs <- many parseAttribute
    symbol '>'
    return (title, attrs)

parseXMLHeaderDoc :: Parser Header
parseXMLHeaderDoc = do
  (tag, attrs) <- parseHeaderStart
  guard (tag == "header")
  content <- parseContentBetween ("</" ++ tag ++ ">")
  buildHeaderFrom tag attrs content

buildHeaderFrom :: String -> [(String, String)] -> String -> Parser Header
buildHeaderFrom tag attrs content =
  let titleValue = lookup "title" attrs
      cA = parseBaliseArgs tag content
      al = extractTag "author" cA
      dateValue = fmap Date (extractTag "date" cA)
  in guard (validTags cA) >>
     parseString ("</" ++ tag ++ ">") >>
     maybe empty (\t -> return (Header t al dateValue)) titleValue

validTags :: [BaliseArg] -> Bool
validTags cA =
  let validTagsList = ["author", "date"]
  in all (\t -> bT t `elem` validTagsList) cA

extractTag :: String -> [BaliseArg] -> Maybe String
extractTag tagName args =
  lookup tagName [(bT a, fromJust (bC a)) | a <- args, isJust (bC a)]

parseDocumentStart :: Parser Balise
parseDocumentStart = do
    symbol '<'
    title <- parseTitle
    guard (title == "document")
    symbol '>'
    return (Balise title Nothing)

parseDocumentEnd :: Parser String
parseDocumentEnd = parseString "</document>"

parseBody :: Parser [Content]
parseBody = do
    parseString ("<body>")
    contents <- many parseContent
    parseString ("</body>")
    return contents

parseXMLDocument :: Parser Document
parseXMLDocument = do
  parseDocumentStart
  header <- parseXMLHeaderDoc
  bodyContent <- parseBody
  parseDocumentEnd
  return $ Document header bodyContent

parseContent :: Parser Content
parseContent =
      try (SectionContent <$> parseSection)
  <|> try (TitleContent <$> parseTitleTag)
  <|> try (LinkContent <$> parseLink)
  <|> try (ListContent <$> parseList)
  <|> try (ParagraphContent <$> parseParagraph)
  <|> try (Text <$> parsePlainText)

parseParagraph :: Parser Paragraph
parseParagraph = do
  parseString "<paragraph>"
  contents <- many parseContent
  parseString "</paragraph>"
  return $ Paragraph contents

parseSection :: Parser Section
parseSection = do
  parseString "<section>"
  mTitle <- optional (TitleContent <$> parseTitleTag)
  contents <- many parseContent
  parseString "</section>"
  return $ Section (case mTitle of
                      Just (TitleContent t) -> Just t
                      _ -> Nothing)
                   contents

parsePlainText :: Parser String
parsePlainText = Parser $ \input ->
  let (txt, rest) = span (/= '<') input
  in if null txt then Nothing else Just (txt, rest)

parseTitleTag :: Parser Title
parseTitleTag = do
  parseString "<title"
  consumeWhitespacesDoc
  parseString "level=\""
  levelStr <- some (satisfy isDigit)
  parseChar '"'
  parseChar '>'
  txt <- parseUntilString "</title>"
  return $ Title (trim txt) (read levelStr)

parseLink :: Parser Link
parseLink = do
  parseString "<link"
  consumeWhitespacesDoc
  parseString "url=\""
  url <- parseUntilChar '"'
  parseChar '"'
  parseChar '>'
  lbl <- many parseContent
  parseString "</link>"
  return $ (Link lbl url)

parseItem :: Parser Item
parseItem = do
  parseString "<item>"
  cs <- many parseContent
  parseString "</item>"
  return $ Item cs

parseList :: Parser List
parseList = do
  parseString "<list>"
  items <- many parseItem
  parseString "</list>"
  return $ List items

parseUntilChar :: Char -> Parser String
parseUntilChar c = Parser $ \input ->
  let (before, rest) = span (/= c) input
  in case rest of
       [] -> Nothing
       (_:rs) -> Just (before, rs)

parseUntilString :: String -> Parser String
parseUntilString end = Parser $ \input ->
  let (prefix, rest) = breakOn end input
  in if null rest then Nothing else Just (prefix, drop (length end) rest)

breakOn :: String -> String -> (String, String)
breakOn needle haystack = go "" haystack
  where
    go acc [] = (acc, "")
    go acc xs@(y:ys)
      | needle `isPrefixOf` xs = (acc, xs)
      | otherwise = go (acc ++ [y]) ys

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing

consumeWhitespacesDoc :: Parser ()
consumeWhitespacesDoc = Parser $ \input ->
  let (_, rest) = span isSpace input
  in Just ((), rest)

try :: Parser a -> Parser a
try (Parser p) = Parser $ \input ->
  case p input of
    Nothing -> Nothing
    success -> success
