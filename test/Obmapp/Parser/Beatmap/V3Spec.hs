{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3Spec where

import Test.Hspec

import Utils
import qualified Obmapp.Beatmap.V3 as B3
import qualified Obmapp.Parser.Beatmap.V3 as P3

spec :: Spec
spec = context "Obmapp.Parser.Beatmap.V3" $ do
    describe "timingPoint" $ do
        it "parses a valid timing point" $ do
            P3.timingPoint `shouldParse` "2500,275.7" `as` B3.TimingPoint
                { B3.offset    = 2500
                , B3.msPerBeat = 275.7}
