{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T
import Test.Hspec

import Lib

shouldParse = runParser
as r e = r `shouldBe` pure (e, T.empty)

main :: IO ()
main = hspec $ do
    describe "Lib.int" $ do
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
    describe "Lib.versionInfo" $ do
        it "parses version 1" $ do
            versionInfo `shouldParse` "osu file format v1" `as` Version 1
        it "parses a significantly newer version" $ do
            versionInfo `shouldParse` "osu file format v13" `as` Version 13
