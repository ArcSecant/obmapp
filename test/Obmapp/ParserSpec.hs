{-# LANGUAGE OverloadedStrings #-}

module Obmapp.ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Utils
import Obmapp.Parser

spec :: Spec
spec = do
    describe "int" $ do
        it "parses 0" $ do
            parse int "" "0" `shouldParse` 0
        it "parses a positive int" $ do
            parse int "" "1" `shouldParse` 1
        it "parses a negative int" $ do
            parse int "" "-1" `shouldParse` (-1)
        it "parses a larger positive int" $ do
            parse int "" "13" `shouldParse` 13
        it "parses a larger negative int" $ do
            parse int "" "-17" `shouldParse` (-17)
        it "doesn't parse a word" $ do
            parse int "" `shouldFailOn` "foobar"
    describe "float" $ do
        it "parses 0" $ do
            parse float "" "0" `shouldParse` 0
        it "parses a positive int" $ do
            parse float "" "1" `shouldParse` 1
        it "parses a negative int" $ do
            parse float "" "-1" `shouldParse` (-1)
        it "parses a larger positive int" $ do
            parse float "" "13" `shouldParse` 13
        it "parses a larger negative int" $ do
            parse float "" "-17" `shouldParse` (-17)
        it "parses a positive decimal number" $ do
            parse float "" "3.14" `shouldParse` 3.14 -- Doubles should be able to represent 3.14 accurately
        it "parses a negative decimal number" $ do
            parse float "" "-4.2" `shouldParse` (-4.2)
        it "doesn't parse a word" $ do
            parse float "" `shouldFailOn` "foobar"
