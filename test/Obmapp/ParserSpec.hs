{-# LANGUAGE OverloadedStrings #-}

module Obmapp.ParserSpec where

import Data.Char (isDigit)
import qualified Data.Text as T
import Test.Hspec

import Utils
import Obmapp.Parser

spec :: Spec
spec = do
    describe "(<?>)" $ do
        it "parses an int and text in the expected order" $ do
            int <?> text "foo" `shouldParse` "42foo" `as` (42, "foo")
        it "parses an int and text in the unexpected order" $ do
            int <?> text "foo" `shouldParse` "foo42" `as` (42, "foo")
        it "parses an int, whitespace, and text in the expected order" $ do
            (int <?> whitespace <?> text "foo") `shouldParse` "42 foo" `as` ((42, " "), "foo")
        it "parses an int, whitespace, and text in an unexpected order" $ do
            (int <?> whitespace <?> text "foo") `shouldParse` " 42foo" `as` ((42, " "), "foo")
        it "parses an int, whitespace, and text in the reverse order" $ do
            (int <?> whitespace <?> text "foo") `shouldParse` "foo 42" `as` ((42, " "), "foo")
    describe "sepBy" $ do
        it "parses a single int value" $ do
            int `sepBy` char ',' `shouldParse` "17" `as` [17]
        it "parses two int values separated by a comma" $ do
            int `sepBy` char ',' `shouldParse` "17,42" `as` [17, 42]
        it "parses three int values separated by a comma" $ do
            int `sepBy` char ',' `shouldParse` "17,42,-1" `as` [17, 42, (-1)]
        it "doesn't parse empty text" $ do
            int `sepBy` char ',' `shouldParse` "" `withError` EndOfInput
    describe "between" $ do
        it "parses a specified char between | symbols" $ do
            between "|" "|" (char 'g') `shouldParse` "|g|" `as` 'g'
        it "parses specified text between brackets" $ do
            between "[" "]" (text "foobar") `shouldParse` "[foobar]" `as` "foobar"
        it "parses an int between parentheses" $ do
            between "(" ")" int `shouldParse` "(12357)" `as` 12357
    describe "int" $ do
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
    describe "float" $ do
        it "parses 0" $ do
            float `shouldParse` "0" `as` 0
        it "parses a positive int" $ do
            float `shouldParse` "1" `as` 1
        it "parses a negative int" $ do
            float `shouldParse` "-1" `as` (-1)
        it "parses a larger positive int" $ do
            float `shouldParse` "13" `as` 13
        it "parses a larger negative int" $ do
            float `shouldParse` "-17" `as` (-17)
        it "parses a positive decimal number" $ do
            float `shouldParse` "3.14" `as` 3.14 -- Float should be able to represent 3.14 accurately
        it "parses a negative decimal number" $ do
            float `shouldParse` "-4.2" `as` (-4.2)
        it "doesn't parse a word" $ do
            float `shouldParse` "foobar" `withError` ConditionNotFulfilled
    describe "text" $ do
        it "parses specified text" $ do
            text "foobar" `shouldParse` "foobar" `as` "foobar"
        it "parses specified text and returns the remaining text" $ do
            text "foo" `shouldParse` "foobar" `asR` ("foo", "bar")
    describe "while" $ do
        it "parses text consisting of digits" $ do
            while isDigit `shouldParse` "12357" `as` "12357"
        it "parses text consisting of spaces" $ do
                while (== ' ') `shouldParse` "   " `as` "   "
