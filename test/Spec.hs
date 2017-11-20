{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
    describe "Lib.parseVersionInfo" $ do
        it "fails" $ do
            parseVersionInfo "" `shouldBe` Nothing
