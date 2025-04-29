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

getString:: String -> String
getString (x:xs)
    | x == '<' = getString xs
    | x == '>' = ""
    | otherwise = [x] ++ getString xs

startXML:: String -> Maybe String
startXML a
    | head a /= '<' || last a /= '>' = Nothing
    | otherwise = Just (getString a)