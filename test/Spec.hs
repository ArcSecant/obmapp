{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Text as T
import Test.Hspec

import Utils
import qualified Obmapp.Beatmap.V3 as B3
import qualified Obmapp.Parser.Beatmap.V3 as P3

import qualified Obmapp.ParserSpec
import qualified Obmapp.Parser.OsuSpec

main :: IO ()
main = hspec $ do
    Obmapp.ParserSpec.spec
    Obmapp.Parser.OsuSpec.spec
    describe "Obmapp.Parser.Beatmap.V3" $ do
        it "parses a valid timing point" $ do
            P3.timingPoint `shouldParse` "2500,275.7" `as` B3.TimingPoint
                { B3.offset    = 2500
                , B3.msPerBeat = 275.7}
