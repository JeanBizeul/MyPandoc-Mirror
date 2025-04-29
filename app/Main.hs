{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Main
-}

import GeneralParse ()
import Options.Applicative
import OptsParsing (opts)

main :: IO ()
main = do
    options <- execParser (info (opts <**> helper) fullDesc)
    print options
