{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Spec
-}

import Test.Hspec

import qualified GeneralParserSpec

main :: IO()
main = hspec $ do
    describe "General Parser" GeneralParserSpec.spec


