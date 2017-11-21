{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T
import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
    describe "Lib.parseVersionInfo" $ do
        it "fails" $ do
            parseVersionInfo "" `shouldBe` Nothing
    describe "Lib.int" $ do
        it "parses 0" $ do
            runParser int "0" `shouldBe` Right (0, T.empty)
        it "parses a positive int" $ do
            runParser int "1" `shouldBe` Right (1, T.empty)
        it "parses a negative int" $ do
            runParser int "-1" `shouldBe` Right (-1, T.empty)
        it "parses a larger positive int" $ do
            runParser int "13" `shouldBe` Right (13, T.empty)
        it "parses a larger negative int" $ do
            runParser int "-17" `shouldBe` Right (-17, T.empty)
