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
            runParser parseSimpleBalise "<balise>" `shouldBe` Just (Balise {baliseTitle = "balise", baliseArgs = Nothing},"")
        it "Double balise name" $ do
            runParser parseSimpleBalise "<balise a>" `shouldBe` Nothing
        it "No <" $ do
            runParser parseSimpleBalise "balise>" `shouldBe` Nothing
        it "No >" $ do
            runParser parseSimpleBalise "<balise" `shouldBe` Nothing
    describe "parseBalise" $ do
        it "correct" $ do
            runParser parseBalise "<balise title=\"example\"></balise>" `shouldBe` Just (Balise {baliseTitle = "balise", baliseArgs = Just [BaliseArg {baliseArgTag = "title", baliseArgContent = Just "example"}]},"")
        it "Without end balise" $ do
            runParser parseBalise "<balise title=\"example\">" `shouldBe` Just (Balise {baliseTitle = "balise", baliseArgs = Just [BaliseArg {baliseArgTag = "title", baliseArgContent = Just "example"}]},"")
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
            runParser parseDoubleBalise "<paragraph>example</paragraph>" `shouldBe` Just (Balise {baliseTitle = "paragraph", baliseArgs = Just [BaliseArg {baliseArgTag = "paragraph", baliseArgContent = Just "example"}]},"")
        it "Balise between balise" $ do
            runParser parseDoubleBalise "<paragraph><test>example</test></paragraph>" `shouldBe` Just (Balise {baliseTitle = "paragraph", baliseArgs = Just [BaliseArg {baliseArgTag = "test", baliseArgContent = Just "example"}]},"")
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
    describe "parseXML simple balise" $ do
        it "Correct" $ do
            runParser parseXML "<balise>" `shouldBe` Just (Balise {baliseTitle = "balise", baliseArgs = Nothing},"")
        it "Double balise name" $ do
            runParser parseXML "<balise a>" `shouldBe` Nothing
        it "Character outside balise" $ do
            runParser parseXML "<balise> a" `shouldBe` Nothing
        it "No <" $ do
            runParser parseXML "balise>" `shouldBe` Nothing
        it "No >" $ do
            runParser parseXML "<balise" `shouldBe` Nothing
    describe "parseXML args balise" $ do
        it "correct" $ do
            runParser parseXML "<balise title=\"example\"></balise>" `shouldBe` Just (Balise {baliseTitle = "balise", baliseArgs = Just [BaliseArg {baliseArgTag = "title", baliseArgContent = Just "example"}]},"")
        it "Without end balise" $ do
            runParser parseXML "<balise title=\"example\">" `shouldBe` Just (Balise {baliseTitle = "balise", baliseArgs = Just [BaliseArg {baliseArgTag = "title", baliseArgContent = Just "example"}]},"")
        it "No <" $ do
            runParser parseXML "balise title=\"example\">" `shouldBe` Nothing
        it "No >" $ do
            runParser parseXML "<balise title=\"example\"" `shouldBe` Nothing
    describe "parseXML double balise" $ do
        it "correct" $ do
            runParser parseXML "<paragraph>example</paragraph>" `shouldBe` Just (Balise {baliseTitle = "paragraph", baliseArgs = Just [BaliseArg {baliseArgTag = "paragraph", baliseArgContent = Just "example"}]},"")
        it "Balise between balise" $ do
            runParser parseXML "<paragraph><test>example</test></paragraph>" `shouldBe` Just (Balise {baliseTitle = "paragraph", baliseArgs = Just [BaliseArg {baliseArgTag = "test", baliseArgContent = Just "example"}]},"")
        it "Without end balise" $ do
            runParser parseXML "<paragraph>example" `shouldBe` Nothing
        it "No < first balise" $ do
            runParser parseXML "paragraph>example</paragraph>" `shouldBe` Nothing
        it "No > first balise" $ do
            runParser parseXML "<paragraphexample</paragraph>" `shouldBe` Nothing
        it "No < second balise" $ do
            runParser parseXML "<paragraph>example/paragraph>" `shouldBe` Nothing
        it "No > second balise" $ do
            runParser parseXML "<paragraph>example</paragraph" `shouldBe` Nothing