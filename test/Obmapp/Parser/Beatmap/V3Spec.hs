{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3Spec where

import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Utils
import qualified Obmapp.Beatmap as B
import qualified Obmapp.Beatmap.V3 as V
import Obmapp.Parser.Beatmap.V3

spec :: Spec
spec = do
    describe "beatmap" $ do
        it "parses a valid beatmap" $ do
            parse beatmap "" sampleBeatmapText `shouldParse` sampleBeatmap
    describe "general" $ do
        it "parses a general section in the expected order" $ do
            parse general "" "[General]\r\nAudioFilename: test.mp3\r\nAudioHash: 12345678\r\n" `shouldParse` V.General
                { V.audioFileName = Just "test.mp3"
                , V.audioHash     = Just "12345678" }
        it "parses a general section the the unexpected order" $ do
            parse general "" "[General]\r\nAudioHash: 12345678\r\nAudioFilename: test.mp3\r\n" `shouldParse` V.General
                { V.audioFileName = Just "test.mp3"
                , V.audioHash     = Just "12345678" }
    describe "metadata" $ do
        it "parses a metadata section in the expected order with all the fields filled" $ do
            parse metadata "" "[Metadata]\r\nTitle:foo\r\nArtist:bar\r\nCreator:foobar\r\nVersion:barfoo\r\n" `shouldParse` V.Metadata
                { V.title   = Just "foo"
                , V.artist  = Just "bar"
                , V.creator = Just "foobar"
                , V.version = Just "barfoo" }
        it "parses a metadata section in the expected order with all but one field filled" $ do
            parse metadata "" "[Metadata]\r\nTitle:foo\r\nArtist:bar\r\nCreator:foobar\r\nVersion:\r\n" `shouldParse` V.Metadata
                { V.title   = Just "foo"
                , V.artist  = Just "bar"
                , V.creator = Just "foobar"
                , V.version = Just "" }
    describe "timingPoint" $ do
        it "parses a valid timing point" $ do
            parse timingPoint "" "2500,275.7" `shouldParse` B.TimingPoint
                { B.offset       = 2500
                , B.beatLength   = 275.7
                , B.meter        = Nothing
                , B.sampleType   = Nothing
                , B.sampleSetIdx = Nothing
                , B.volume       = Nothing
                , B.uninherited  = Nothing
                , B.kiaiMode     = Nothing }
    describe "events" $ do
        it "parses an empty events section" $ do
            parse events "" "[Events]\r\n" `shouldParse` ()
        it "parses an events section containing nonsense" $ do
            parse events "" "[Events]\r\nfoobar" `shouldParse` ()
    describe "hitObjects" $ do
        it "parses a valid hit objects section" $ do
            parse hitObjects "" sampleHitObjectsSectionText `shouldParse` sampleHitObjects

sampleBeatmap :: V.Beatmap
sampleBeatmap = V.Beatmap
    { V.general = V.General
        { V.audioFileName = Just "Sample.mp3"
        , V.audioHash     = Just "12345678901234567890123456789012" }
    , V.metadata = V.Metadata
        { V.title   = Just "foo"
        , V.artist  = Just "bar"
        , V.creator = Just "foobar"
        , V.version = Just "" }
    , V.difficulty = V.Difficulty
        { V.hpDrainRate       = Just 6
        , V.circleSize        = Just 4
        , V.overallDifficulty = Just 6
        , V.sliderMultiplier  = Just 1.4
        , V.sliderTickRate    = Just 1 }
    , V.timingPoints = [ B.TimingPoint
        { B.offset       = 2000
        , B.beatLength   = 173.8
        , B.meter        = Nothing
        , B.sampleType   = Nothing
        , B.sampleSetIdx = Nothing
        , B.volume       = Nothing
        , B.uninherited  = Nothing
        , B.kiaiMode     = Nothing } ]
    , V.hitObjects   = sampleHitObjects }

sampleHitObjects =
    [ B.HitObject
            { B.position = (100, 200)
            , B.time     = 2500
            , B.newCombo = Nothing
            , B.hitSound = B.HitSound
                { B.normalHitSound  = True
                , B.whistleHitSound = False
                , B.finishHitSound  = False
                , B.clapHitSound    = False }
            , B.details = B.HitCircle
            , B.extras   = Nothing }
    , B.HitObject
        { B.position = (50, 200)
        , B.time     = 3000
        , B.newCombo = Nothing
        , B.hitSound = B.HitSound
            { B.normalHitSound  = False
            , B.whistleHitSound = True
            , B.finishHitSound  = False
            , B.clapHitSound    = False }
        , B.details = B.Slider
            { B.sliderShape = B.Bezier
                [ [ ( 32, 192)
                    , ( 32, 384)
                    , (480, 384)
                    , (480, 160) ] ]
            , B.edgeInfo    = B.EdgeInfo
                { B.repeats = 3
                , B.hitSoundsAndAdditions = [] } -- Well this is funny... Wasn't supposed to be able to be empty!
            , B.pixelLength = 560 }
        , B.extras   = Nothing }
    , B.HitObject
        { B.position = (40, 150)
        , B.time     = 5000
        , B.newCombo = Just 0
        , B.hitSound = B.HitSound
            { B.normalHitSound  = False
            , B.whistleHitSound = False
            , B.finishHitSound  = True
            , B.clapHitSound    = False }
        , B.details = B.Slider
            { B.sliderShape = B.Catmull
                [ (160, 160)
                , (128, 32 )
                , (384, 32 )
                , (320, 192) ]
            , B.edgeInfo    = B.EdgeInfo
                { B.repeats = 3
                , B.hitSoundsAndAdditions = [] }
            , B.pixelLength = 560 }
        , B.extras   = Nothing }
    , B.HitObject
        { B.position = (250, 100)
        , B.time     = 7000
        , B.newCombo = Nothing
        , B.hitSound = B.HitSound
            { B.normalHitSound  = False
            , B.whistleHitSound = False
            , B.finishHitSound  = False
            , B.clapHitSound    = True }
        , B.details = B.Slider
            { B.sliderShape = B.Linear
                [ (320,  96)
                , (162,  95)
                , (160, 322)
                , (352, 320) ]
            , B.edgeInfo    = B.EdgeInfo
                { B.repeats = 1
                , B.hitSoundsAndAdditions = [] }
            , B.pixelLength = 560 }
        , B.extras   = Nothing }
    , B.HitObject
        { B.position = (300, 50)
        , B.time     = 9000
        , B.newCombo = Just 0
        , B.hitSound = B.HitSound
            { B.normalHitSound  = False
            , B.whistleHitSound = False
            , B.finishHitSound  = False
            , B.clapHitSound    = False }
        , B.details = B.Spinner { B.endTime = 11000 }
        , B.extras  = Nothing } ]

sampleBeatmapText =
    "[General]\r\n\
    \AudioFilename: Sample.mp3\r\n\
    \AudioHash: 12345678901234567890123456789012\r\n\
    \\r\n\
    \[Metadata]\r\n\
    \Title:foo\r\n\
    \Artist:bar\r\n\
    \Creator:foobar\r\n\
    \Version:\r\n\
    \\r\n\
    \[Difficulty]\r\n\
    \HPDrainRate:6\r\n\
    \CircleSize:4\r\n\
    \OverallDifficulty:6\r\n\
    \SliderMultiplier: 1.4\r\n\
    \SliderTickRate: 1\r\n\
    \\r\n\
    \[Events]\r\n\
    \foobar, because this isn't supported yet\r\n\
    \\r\n\
    \[TimingPoints]\r\n\
    \2000,173.8\r\n\
    \\r\n" `T.append` sampleHitObjectsSectionText

sampleHitObjectsSectionText =
    "[HitObjects]\r\n\
    \100,200,2500,1,1,\r\n\
    \50,200,3000,2,2,B|32:192|32:384|480:384|480:160,3,560\r\n\
    \40,150,5000,6,4,C|160:160|128:32|384:32|320:192,3,560\r\n\
    \250,100,7000,2,8,L|320:96|162:95|160:322|352:320,1,560\r\n\
    \300,50,9000,12,0,11000\r\n"