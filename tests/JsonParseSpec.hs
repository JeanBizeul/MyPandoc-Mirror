{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- JsonParseSpec
-}

module JsonParseSpec where

import Test.Hspec
import GeneralParse (runParser)
import JsonParse

spec :: Spec
spec = do
    describe "parseWhiteSpace" $ do
        it "Correct whitespace" $ do
            runParser parseWhiteSpace "   abc" `shouldBe` Just ((), "abc")
        it "No whitespace" $ do
            runParser parseWhiteSpace "abc" `shouldBe` Just ((), "abc")
        it "Empty string" $ do
            runParser parseWhiteSpace "" `shouldBe` Just ((), "")

    describe "parseNull" $ do
        it "Correct null" $ do
            runParser parseNull "null" `shouldBe` Just (JsonNull, "")
        it "Incorrect null" $ do
            runParser parseNull "nul abc" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseNull "" `shouldBe` Nothing

    describe "parseBool" $ do
        it "Correct true" $ do
            runParser parseBool "true" `shouldBe` Just (JsonBool True, "")
        it "Correct false" $ do
            runParser parseBool "false" `shouldBe` Just (JsonBool False, "")
        it "Incorrect bool" $ do
            runParser parseBool "tru abc" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseBool "" `shouldBe` Nothing

    describe "parseNumber" $ do
        it "Correct float" $ do
            runParser parseNumber "123.456" `shouldBe` Just (JsonNumber 123.456, "")
        it "Correct negative float" $ do
            runParser parseNumber "-123.456" `shouldBe` Just (JsonNumber (-123.456), "")
        it "Correct integer" $ do
            runParser parseNumber "123" `shouldBe` Just (JsonNumber 123, "")
        it "Correct negative integer" $ do
            runParser parseNumber "-123" `shouldBe` Just (JsonNumber (-123), "")
        it "Incorrect number" $ do
            runParser parseNumber "abc" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseNumber "" `shouldBe` Nothing


    describe "parseJsonString" $ do
        it "Correct string" $ do
            runParser parseJsonString "\"Hello\"" `shouldBe` Just (JsonString "Hello", "")
        it "Correct string with escape and space" $ do
            runParser parseJsonString "\"Hello World\"" `shouldBe` Just (JsonString "Hello World", "")
        it "Correct string with escape" $ do
            runParser parseJsonString "\"Hello\\nWorld\"" `shouldBe` Just (JsonString "Hello\nWorld","")
        it "Incorrect string" $ do
            runParser parseJsonString "\"Hello" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseJsonString "" `shouldBe` Nothing
    
    describe "parseJsonArray" $ do
        it "Correct array with numbers" $ do
            runParser parseJsonArray "[1, 2, 3]" `shouldBe` Just (JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3], "")
        it "Correct array with whitespace and numbers" $ do
            runParser parseJsonArray "[ 1 , 2 , 3 ]" `shouldBe` Just (JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3], "")
        it "Correct array with strings" $ do
            runParser parseJsonArray "[\"Hello\", \"World\"]" `shouldBe` Just (JsonArray [JsonString "Hello", JsonString "World"], "")
        it "Correct array with mixed types" $ do
            runParser parseJsonArray "[1, \"Hello\", true]" `shouldBe` Just (JsonArray [JsonNumber 1, JsonString "Hello", JsonBool True], "")
        it "Correct empty array" $ do
            runParser parseJsonArray "[]" `shouldBe` Just (JsonArray [], "")
        it "Incorrect array" $ do
            runParser parseJsonArray "[1, 2, ]" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseJsonArray "" `shouldBe` Nothing

    describe "parseJsonObject" $ do
        it "Correct object with numbers" $ do
            runParser parseJsonObject "{\"key\": 1, \"key2\": 2}" `shouldBe` Just (JsonObject [("key", JsonNumber 1), ("key2", JsonNumber 2)], "")
        it "Correct object with strings" $ do
            runParser parseJsonObject "{\"key\": \"value\"}" `shouldBe` Just (JsonObject [("key", JsonString "value")], "")
        it "Correct object with mixed types" $ do
            runParser parseJsonObject "{\"key\": 1, \"key2\": \"value\"}" `shouldBe` Just (JsonObject [("key", JsonNumber 1), ("key2", JsonString "value")], "")
        it "Correct empty object" $ do
            runParser parseJsonObject "{}" `shouldBe` Just (JsonObject [], "")
        it "Incorrect object" $ do
            runParser parseJsonObject "{\"key\": 1, }" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseJsonObject "" `shouldBe` Nothing
    
    describe "parseJsonKey" $ do
        it "Correct key with no escape" $ do
            runParser parseJsonKey "\"key\"" `shouldBe` Just ("key", "")
        it "Correct key with escape character" $ do
            runParser parseJsonKey "\"key\\\"value\"" `shouldBe` Just ("key\"value", "")
        it "Correct key with spaces" $ do
            runParser parseJsonKey "\"key value\"" `shouldBe` Just ("key value", "")
        it "Incorrect key with missing closing quote" $ do
            runParser parseJsonKey "\"key" `shouldBe` Nothing
        it "Incorrect key with invalid characters" $ do
            runParser parseJsonKey "{key: 1}" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseJsonKey "" `shouldBe` Nothing

    describe "parseJsonValue" $ do
        it "Correct number" $ do
            runParser parseJsonValue "123" `shouldBe` Just (JsonNumber 123, "")
        it "Correct string" $ do
            runParser parseJsonValue "\"Hello\"" `shouldBe` Just (JsonString "Hello", "")
        it "Correct boolean" $ do
            runParser parseJsonValue "true" `shouldBe` Just (JsonBool True, "")
        it "Correct null" $ do
            runParser parseJsonValue "null" `shouldBe` Just (JsonNull, "")
        it "Correct array" $ do
            runParser parseJsonValue "[1, 2, 3]" `shouldBe` Just (JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3], "")
        it "Correct object" $ do
            runParser parseJsonValue "{\"key\": 1}" `shouldBe` Just (JsonObject [("key", JsonNumber 1)], "")
        it "Incorrect value" $ do
            runParser parseJsonValue "{key: 1}" `shouldBe` Nothing
        it "Empty string" $ do
            runParser parseJsonValue "" `shouldBe` Nothing