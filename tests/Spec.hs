{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- Spec
-}

import Test.Hspec

main :: IO()
main = hspec $ do
    describe "Basic math" $ do
        it "1 + 1 == 2" $ do
            1 + 1 `shouldBe` 2
