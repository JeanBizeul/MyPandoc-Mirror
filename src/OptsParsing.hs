{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- OptsParsing
-}

module OptsParsing (Options(..), FileFormat(..), opts) where

import Options.Applicative (Parser, strOption, long, short, metavar, help, optional, execParser, info, fullDesc, eitherReader, option, ReadM)

data FileFormat = Markdown | XML | JSON
  deriving (Show, Eq)

stringToFileFormat :: String -> Maybe FileFormat
stringToFileFormat "markdown" = Just Markdown
stringToFileFormat "xml"     = Just XML
stringToFileFormat "json"    = Just JSON
stringToFileFormat _         = Nothing

data Options = Options {
    inputFilePath :: String,
    outputFileFormat :: FileFormat,
    outputFilePath :: Maybe String,
    inputFileFormat :: Maybe FileFormat
} deriving (Show)

fileFormatReader :: ReadM FileFormat
fileFormatReader = eitherReader $ \arg -> case stringToFileFormat arg of
        Just format -> Right format
        Nothing     -> Left $ "Invalid file format: " ++ arg

opts :: Parser Options
opts = Options
    <$> inputFileParser
    <*> outputFormatParser
    <*> outputFileParser
    <*> inputFormatParser

inputFileParser :: Parser String
inputFileParser = strOption
    ( long "input"
    <> short 'i'
    <> metavar "FILEPATH"
    <> help "Path to the input file" )

outputFormatParser :: Parser FileFormat
outputFormatParser = option fileFormatReader
    ( long "output-format"
    <> short 'f'
    <> metavar "FORMAT"
    <> help "Output file format (markdown, XML or JSON)" )

outputFileParser :: Parser (Maybe String)
outputFileParser =  optional $ strOption
    ( long "output"
    <> short 'o'
    <> metavar "FILEPATH"
    <> help "Path to the output file" )

inputFormatParser :: Parser (Maybe FileFormat)
inputFormatParser =  optional $ option fileFormatReader
        ( long "input-format"
        <> short 'e'
        <> metavar "FORMAT"
        <> help "Input file format (markdown, XML or JSON)" )
