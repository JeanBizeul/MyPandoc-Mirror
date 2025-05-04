{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Main
-}

import GeneralParse ()
import Options.Applicative (execParser, info, helper, fullDesc, (<**>))
import OptsParsing (
    opts,
    Options (inputFilePath, inputFileFormat, outputFilePath, outputFileFormat),
    FileFormat (Markdown, XML, JSON))
import Document (Document (body, Document, header), Header (Header, title, date, author))
import System.Exit (exitWith)
import GHC.IO.Exception (ExitCode(ExitFailure))
import MarkdownParse (parseMarkdown)
import MarkdownWrite (writeMarkdown)

parseJSON :: String -> Maybe Document
parseJSON _ = Just (Document {
    header=Header{title="JSON title", date=Nothing, author=Nothing},
    body=[]})

parseXML :: String -> Maybe Document
parseXML _ = Just (Document {
    header=Header {title="XML title", date=Nothing, author=Nothing},
    body=[]})

writeJSON :: Document -> Maybe String
writeJSON _ = Just "JSON output"

writeXML :: Document -> Maybe String
writeXML _ = Just "XML output"

writeDocument :: Document -> FileFormat -> Maybe String
writeDocument doc format = case format of
    JSON     -> writeJSON doc
    XML      -> writeXML doc
    Markdown -> writeMarkdown doc

parseFile :: String -> Maybe FileFormat -> Maybe Document
parseFile content (Just format) = case format of
    JSON     -> parseJSON content
    XML      -> parseXML content
    Markdown -> parseMarkdown content
parseFile content Nothing = parseMarkdown content-- parseJSON content <|> parseXML content <|> parseMarkdown content

main :: IO ()
main = do
    options <- execParser (info (opts <**> helper) fullDesc)
    inputFile <- readFile (inputFilePath options)
    case parseFile inputFile (inputFileFormat options) of
        Nothing -> exitWithError "Failed to parse the input file."
        Just doc -> case writeDocument doc (outputFileFormat options) of
            Nothing -> exitWithError "Failed to generate the output file."
            Just output -> case outputFilePath options of
                Just path -> writeFile path output
                Nothing   -> putStrLn output

exitWithError :: String -> IO ()
exitWithError msg = putStrLn msg >> exitWith (ExitFailure 84)
