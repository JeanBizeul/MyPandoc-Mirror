{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- XMLParseSpec
-}

module XMLParseSpec where

import Test.Hspec
import GeneralParse (runParser)
import XMLParse

spec :: Spec
spec = do
    describe "parseSimpleBalise" $ do
        it "Simple balise" $ do
            runParser parseSimpleBalise "</header>" `shouldBe` Just (Balise {baliseTitle = "/header", baliseArgs = Nothing},"")
        it "Incorrect" $ do
            runParser parseSimpleBalise "</header a>" `shouldBe` Just (Balise {baliseTitle = "/header", baliseArgs = Nothing}," a")
        it "No <" $ do
            runParser parseSimpleBalise "/header>" `shouldBe` Nothing
        it "No >" $ do
            runParser parseSimpleBalise "</header" `shouldBe` Nothing