{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import qualified Obmapp.ParserSpec
import qualified Obmapp.Parser.Beatmap.V3Spec
import qualified Obmapp.Parser.OsuSpec

main :: IO ()
main = hspec $ do
    Obmapp.ParserSpec.spec
    Obmapp.Parser.Beatmap.V3Spec.spec
    Obmapp.Parser.OsuSpec.spec
