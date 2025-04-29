{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- OptsParsing
-}

module OptsParsing (Options(..), opts) where

import Options.Applicative (Parser, strOption, long, short, metavar, help, optional, execParser, info, fullDesc)

data Options = Options {
    inputFilePath :: String,
    outputFileFormat :: String,
    outputFilePath :: Maybe String,
    inputFileFormat :: Maybe String
} deriving (Show)

opts :: Parser Options
opts = Options
    <$> strOption
        ( long "input"
        <> short 'i'
        <> metavar "FILEPATH"
        <> help "Path to the input file" )
    <*> strOption
        ( long "output-format"
        <> short 'f'
        <> metavar "FORMAT"
        <> help "Output file format (markdown, XML or JSON)" )
    <*> optional (strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILEPATH"
        <> help "Path to the output file" ))
    <*> optional (strOption
        ( long "input-format"
        <> short 'e'
        <> metavar "FORMAT"
        <> help "Input file format (markdown, XML or JSON)" ))