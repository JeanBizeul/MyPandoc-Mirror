{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Main
-}

import GeneralParse (runParser)
import JsonParse (parseJsonValue, jsonToDocument)
import Document
import Options.Applicative (execParser, info, helper, fullDesc, (<**>))
import Control.Applicative ((<|>))
import OptsParsing (
    opts,
    Options (inputFilePath, inputFileFormat, outputFilePath, outputFileFormat),
    FileFormat (Markdown, XML, JSON))
import System.Exit (exitWith)
import GHC.IO.Exception (ExitCode(ExitFailure))
import JsonWriter (documentToJson)
import XMLParse
import XMLWriter
import MarkdownParse (parseMarkdown)
import MarkdownWrite (writeMarkdown)

parseJSON :: String -> Maybe Document
parseJSON str =
    case runParser parseJsonValue str of
        Just (jsonVal, rest)
            | all (`elem` " \n\r\t") rest ->
                jsonToDocument jsonVal
        _ -> Nothing

parseXML :: String -> Maybe Document
parseXML str = case runParser parseXMLDocument str of
    Just (doc, _) -> Just doc
    Nothing       -> Nothing

writeJSON :: Document -> Maybe String
writeJSON doc = Just (documentToJson doc)

writeXML :: Document -> Maybe String
writeXML doc = Just (documentToXML doc)

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
parseFile content Nothing =
    parseJSON content
    <|> parseXML content
    <|> parseMarkdown content

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
