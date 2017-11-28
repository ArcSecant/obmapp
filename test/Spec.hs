{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Text as T
import Test.Hspec

import Obmapp.Parser
import Obmapp.Parser.Osu

shouldParse = runParser
as r e = r `shouldBe` pure (e, T.empty)
asR r e = r `shouldBe` pure e
withError r e = r `shouldBe` Left [e]

main :: IO ()
main = hspec $ do
    describe "Obmapp.Parser.between" $ do
        it "parses a specified char between | symbols" $ do
            between "(" ")" (char 'g') `shouldParse` "|g|" `as` 'g'
        it "parses specified text between brackets" $ do
            between "(" ")" (text "foobar") `shouldParse` "[foobar]" `as` "foobar"
        it "parses an int between parentheses" $ do
            between "(" ")" int `shouldParse` "(12357)" `as` 12357
    describe "Obmapp.Parser.int" $ do
        it "parses 0" $ do
            int `shouldParse` "0" `as` 0
        it "parses a positive int" $ do
            int `shouldParse` "1" `as` 1
        it "parses a negative int" $ do
            int `shouldParse` "-1" `as` (-1)
        it "parses a larger positive int" $ do
            int `shouldParse` "13" `as` 13
        it "parses a larger negative int" $ do
            int `shouldParse` "-17" `as` (-17)
        it "doesn't parse a word" $ do
            int `shouldParse` "foobar" `withError` ConditionNotFulfilled
    describe "Obmapp.Parser.text" $ do
        it "parses specified text" $ do
            text "foobar" `shouldParse` "foobar" `as` "foobar"
        it "parses specified text and returns the remaining text" $ do
            text "foo" `shouldParse` "foobar" `asR` ("foo", "bar")
    describe "Obmapp.Parser.while" $ do
        it "parses text consisting of digits" $ do
            while isDigit `shouldParse` "12357" `as` "12357"
        it "parses text consisting of spaces" $ do
            while (== ' ') `shouldParse` "   " `as` "   "
    describe "Obmapp.Parser.Osu.versionInfo" $ do
        it "parses version 1" $ do
            versionInfo `shouldParse` "osu file format v1" `as` Version 1
        it "parses a significantly newer version" $ do
            versionInfo `shouldParse` "osu file format v13" `as` Version 13
        it "doesn't parse version 0" $ do
            versionInfo `shouldParse` "osu file format v0" `withError` ConditionNotFulfilled
        it "doesn't parse a negative version" $ do
            versionInfo `shouldParse` "osu file format v-1" `withError` ConditionNotFulfilled
