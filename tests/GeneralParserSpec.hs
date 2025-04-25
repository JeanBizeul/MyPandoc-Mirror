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
    
    describe "parseAnyChar" $ do
        it "Correct char" $ do
            runParser (parseAnyChar "a") "abc" `shouldBe` Just ('a', "bc")
        it "Correct multiple char" $ do
            runParser (parseAnyChar "ba") "abc" `shouldBe` Just ('a', "bc")
        it "Wrong char in str" $ do
            runParser (parseAnyChar "b") "abc" `shouldBe` Nothing
        it "Wrong multiple char in str" $ do
            runParser (parseAnyChar "bc") "abc" `shouldBe` Nothing
        it "Wrong char not in str" $ do
            runParser (parseAnyChar "z") "abc" `shouldBe` Nothing
        it "Wrong multiple char not in str" $ do
            runParser (parseAnyChar "xyz") "abc" `shouldBe` Nothing
        it "Empty str" $ do
            runParser (parseAnyChar "a") "" `shouldBe` Nothing
        it "Empty str'" $ do
            runParser (parseAnyChar "") "abc" `shouldBe` Nothing
        it "Empty str''" $ do
            runParser (parseAnyChar "") "" `shouldBe` Nothing
        it "Empty str'''" $ do
            runParser (parseAnyChar "abc") "" `shouldBe` Nothing

    describe "ParseDigit" $ do
        it "Correct digit" $ do
            runParser parseDigit "123" `shouldBe` Just ('1', "23")
        it "No Digit" $ do
            runParser parseDigit "abc" `shouldBe` Nothing
        it "Empty str" $ do
            runParser parseDigit "" `shouldBe` Nothing

    describe "ParseUInt" $ do
        it "Correct digit" $ do
            runParser parseUInt "123" `shouldBe` Just (123, "")
        it "Leading zeros" $ do
            runParser parseUInt "000123" `shouldBe` Just (123, "")
        it "No Digit" $ do
            runParser parseUInt "abc" `shouldBe` Nothing
        it "Empty str" $ do
            runParser parseUInt "" `shouldBe` Nothing
        it "Negative number" $ do
            runParser parseUInt "-123" `shouldBe` Nothing

    describe "ParseInt" $ do
        it "Correct digit" $ do
            runParser parseInt "123" `shouldBe` Just (123, "")
        it "Leading zeros" $ do
            runParser parseInt "000123" `shouldBe` Just (123, "")
        it "Negative number" $ do
            runParser parseInt "-123" `shouldBe`Just (-123, "")
        it "No Digit" $ do
            runParser parseInt "abc" `shouldBe` Nothing
        it "Empty str" $ do
            runParser parseInt "" `shouldBe` Nothing

    describe "ParseTuple" $ do
        it "Correct tuple" $ do
            runParser (parseTuple parseInt) "(1,2)" `shouldBe` Just ((1, 2), "")
        it "Empty tuple" $ do
            runParser (parseTuple parseInt) "(,)" `shouldBe` Nothing
        it "No comma" $ do
            runParser (parseTuple parseInt) "(1 2)" `shouldBe` Nothing
        it "No closing paren" $ do
            runParser (parseTuple parseInt) "(1,2" `shouldBe` Nothing
        it "No opening paren" $ do
            runParser (parseTuple parseInt) "1,2)" `shouldBe` Nothing
        it "Empty str" $ do
            runParser (parseTuple parseInt) "" `shouldBe` Nothing
        it "Wrong parser type" $ do
            runParser (parseTuple parseInt) "(1,2.5)" `shouldBe` Nothing

    describe "ParseTruple" $ do
        it "Correct truple" $ do
            runParser (parseTruple parseInt) "(1,2,3)" `shouldBe` Just ((1, 2, 3), "")
        it "Empty truple" $ do
            runParser (parseTruple parseInt) "(,,)" `shouldBe` Nothing
        it "No comma" $ do
            runParser (parseTruple parseInt) "(1 2 3)" `shouldBe` Nothing
        it "No closing paren" $ do
            runParser (parseTruple parseInt) "(1,2,3" `shouldBe` Nothing
        it "No opening paren" $ do
            runParser (parseTruple parseInt) "1,2,3)" `shouldBe` Nothing
        it "Empty str" $ do
            runParser (parseTruple parseInt) "" `shouldBe` Nothing
        it "Wrong parser type" $ do
            runParser (parseTruple parseInt) "(1,2.5,3)" `shouldBe` Nothing

    describe "parseString" $ do
        it "Correct string" $ do
            runParser (parseString "abc") "abc" `shouldBe` Just ("abc", "")
        it "Wrong string" $ do
            runParser (parseString "abc") "def" `shouldBe` Nothing
        it "Empty str" $ do
            runParser (parseString "abc") "" `shouldBe` Nothing
        it "Partial match" $ do
            runParser (parseString "abc") "abcd" `shouldBe` Just ("abc", "d")
        it "Leading spaces" $ do
            runParser (parseString "abc") "  abc" `shouldBe` Nothing