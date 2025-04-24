{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- GeneralParserSpec
-}

module GeneralParserSpec where

import Test.Hspec
import GeneralParse

spec :: Spec
spec = do
    describe "parseChar" $ do
        it "Correct char" $ do
            runParser (parseChar 'a') "abc" `shouldBe` Just ('a', "bc")
        it "Wrong char in str" $ do
            runParser (parseChar 'b') "abc" `shouldBe` Nothing
        it "Wrong char not in str" $ do
            runParser (parseChar 'z') "abc" `shouldBe` Nothing
        it "Empty str" $ do
            runParser (parseChar 'a') "" `shouldBe` Nothing
