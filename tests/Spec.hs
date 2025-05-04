{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Spec
-}

import Test.Hspec

import qualified GeneralParserSpec
import qualified JsonParseSpec
import qualified XMLParseSpec

main :: IO()
main = hspec $ do
    describe "General Parser" GeneralParserSpec.spec
    describe "Json Parser" JsonParseSpec.spec
    describe "XML Parser" XMLParseSpec.spec
