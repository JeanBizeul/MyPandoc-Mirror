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
            runParser parseSimpleBalise "<balise>" `shouldBe` Just (Balise {balTit = "balise", balArg = Nothing},"")
        it "Double balise name" $ do
            runParser parseSimpleBalise "<balise a>" `shouldBe` Nothing
        it "No <" $ do
            runParser parseSimpleBalise "balise>" `shouldBe` Nothing
        it "No >" $ do
            runParser parseSimpleBalise "<balise" `shouldBe` Nothing
    describe "parseBalise" $ do
        it "correct" $ do
            runParser parseBalise "<balise title=\"example\"></balise>" `shouldBe` Just (Balise {balTit = "balise", balArg = Just [BaliseArg {bT = "title", bC = Just "example"}]},"")
        it "Without end balise" $ do
            runParser parseBalise "<balise title=\"example\">" `shouldBe` Just (Balise {balTit = "balise", balArg = Just [BaliseArg {bT = "title", bC = Just "example"}]},"")
        it "No <" $ do
            runParser parseBalise "balise title=\"example\">" `shouldBe` Nothing
        it "No >" $ do
            runParser parseBalise "<balise title=\"example\"" `shouldBe` Nothing
        it "On a simple balise" $ do
            runParser parseBalise "<balise>" `shouldBe` Nothing
        it "On a double balise" $ do
            runParser parseBalise "<paragraph>example</paragraph>" `shouldBe` Nothing
    describe "parseDoubleBalise" $ do
        it "correct" $ do
            runParser parseDoubleBalise "<paragraph>example</paragraph>" `shouldBe` Just (Balise {balTit = "paragraph", balArg = Just [BaliseArg {bT = "paragraph", bC = Just "example"}]},"")
        it "Balise between balise" $ do
            runParser parseDoubleBalise "<paragraph><test>example</test></paragraph>" `shouldBe` Just (Balise {balTit = "paragraph", balArg = Just [BaliseArg {bT = "test", bC = Just "example"}]},"")
        it "Without end balise" $ do
            runParser parseDoubleBalise "<paragraph>example" `shouldBe` Nothing
        it "No < first balise" $ do
            runParser parseDoubleBalise "paragraph>example</paragraph>" `shouldBe` Nothing
        it "No > first balise" $ do
            runParser parseDoubleBalise "<paragraphexample</paragraph>" `shouldBe` Nothing
        it "No < second balise" $ do
            runParser parseDoubleBalise "<paragraph>example/paragraph>" `shouldBe` Nothing
        it "No > second balise" $ do
            runParser parseDoubleBalise "<paragraph>example</paragraph" `shouldBe` Nothing
        it "On a simple balise" $ do
            runParser parseDoubleBalise "<balise>" `shouldBe` Nothing
        it "On a double balise" $ do
            runParser parseDoubleBalise "<balise title=\"example\"></balise>" `shouldBe` Nothing
    describe "parseXMLbalise simple balise" $ do
        it "Correct" $ do
            runParser parseXMLbalise "<balise>" `shouldBe` Just (Balise {balTit = "balise", balArg = Nothing},"")
        it "Double balise name" $ do
            runParser parseXMLbalise "<balise a>" `shouldBe` Nothing
        it "Character outside balise" $ do
            runParser parseXMLbalise "<balise> a" `shouldBe` Nothing
        it "No <" $ do
            runParser parseXMLbalise "balise>" `shouldBe` Nothing
        it "No >" $ do
            runParser parseXMLbalise "<balise" `shouldBe` Nothing
    describe "parseXMLbalise args balise" $ do
        it "correct" $ do
            runParser parseXMLbalise "<balise title=\"example\"></balise>" `shouldBe` Just (Balise {balTit = "balise", balArg = Just [BaliseArg {bT = "title", bC = Just "example"}]},"")
        it "Without end balise" $ do
            runParser parseXMLbalise "<balise title=\"example\">" `shouldBe` Just (Balise {balTit = "balise", balArg = Just [BaliseArg {bT = "title", bC = Just "example"}]},"")
        it "No <" $ do
            runParser parseXMLbalise "balise title=\"example\">" `shouldBe` Nothing
        it "No >" $ do
            runParser parseXMLbalise "<balise title=\"example\"" `shouldBe` Nothing
    describe "parseXMLbalise double balise" $ do
        it "correct" $ do
            runParser parseXMLbalise "<paragraph>example</paragraph>" `shouldBe` Just (Balise {balTit = "paragraph", balArg = Just [BaliseArg {bT = "paragraph", bC = Just "example"}]},"")
        it "Balise between balise" $ do
            runParser parseXMLbalise "<paragraph><test>example</test></paragraph>" `shouldBe` Just (Balise {balTit = "paragraph", balArg = Just [BaliseArg {bT = "test", bC = Just "example"}]},"")
        it "Without end balise" $ do
            runParser parseXMLbalise "<paragraph>example" `shouldBe` Nothing
        it "No < first balise" $ do
            runParser parseXMLbalise "paragraph>example</paragraph>" `shouldBe` Nothing
        it "No > first balise" $ do
            runParser parseXMLbalise "<paragraphexample</paragraph>" `shouldBe` Nothing
        it "No < second balise" $ do
            runParser parseXMLbalise "<paragraph>example/paragraph>" `shouldBe` Nothing
        it "No > second balise" $ do
            runParser parseXMLbalise "<paragraph>example</paragraph" `shouldBe` Nothing