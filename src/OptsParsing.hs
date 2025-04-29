{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- OptsParsing
-}

module OptsParsing (Options(..), opts) where

import Options.Applicative (Parser, strOption, long, short, metavar, help, optional, execParser, info, fullDesc, eitherReader, option, ReadM)

data FileFormat = Markdown | XML | JSON
  deriving (Show, Eq)

stringToFileFormat :: String -> Maybe FileFormat
stringToFileFormat "markdown" = Just Markdown
stringToFileFormat "xml"     = Just XML
stringToFileFormat "json"    = Just JSON
stringToFileFormat _         = Nothing

fileFormatReader :: ReadM FileFormat
fileFormatReader = eitherReader $ \arg -> case stringToFileFormat arg of
        Just format -> Right format
        Nothing     -> Left $ "Invalid file format: " ++ arg

data Options = Options {
    inputFilePath :: String,
    outputFileFormat :: FileFormat,
    outputFilePath :: Maybe String,
    inputFileFormat :: Maybe FileFormat
} deriving (Show)

opts :: Parser Options
opts = Options
    <$> strOption
        ( long "input"
        <> short 'i'
        <> metavar "FILEPATH"
        <> help "Path to the input file" )

    <*> option fileFormatReader
        ( long "output-format"
        <> short 'f'
        <> metavar "FORMAT"
        <> help "Output file format (markdown, XML or JSON)" )

    <*> optional (strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILEPATH"
        <> help "Path to the output file" ))

    <*> optional (option fileFormatReader
        ( long "input-format"
        <> short 'e'
        <> metavar "FORMAT"
        <> help "Input file format (markdown, XML or JSON)" ))