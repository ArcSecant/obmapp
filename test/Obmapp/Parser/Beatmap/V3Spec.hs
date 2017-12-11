{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3Spec where

import Test.Hspec

import Utils
import qualified Obmapp.Beatmap.V3 as B
import Obmapp.Parser.Beatmap.V3

spec :: Spec
spec = do
    describe "general" $ do
        it "parses a general section in the expected order" $ do
            general `shouldParse` "[General]\r\nAudioFilename: test.mp3\r\nAudioHash: 12345678\r\n" `as` B.General
                { B.audioFileName = Just "test.mp3"
                , B.audioHash     = Just "12345678" }
        it "parses a general section the the unexpected order" $ do
            general `shouldParse` "[General]\r\nAudioHash: 12345678\r\nAudioFilename: test.mp3\r\n" `as` B.General
                { B.audioFileName = Just "test.mp3"
                , B.audioHash     = Just "12345678" }
    describe "timingPoint" $ do
        it "parses a valid timing point" $ do
            timingPoint `shouldParse` "2500,275.7" `as` B.TimingPoint
                { B.offset    = 2500
                , B.msPerBeat = 275.7}
