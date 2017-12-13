{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3Spec where

import Test.Hspec

import Utils
import qualified Obmapp.Beatmap as B
import qualified Obmapp.Beatmap.V3 as V
import Obmapp.Parser.Beatmap.V3

spec :: Spec
spec = do
    describe "beatmap" $ do
        it "parses a valid beatmap" $ do
            beatmap `shouldParse` sampleBeatmapText `as` sampleBeatmap
    describe "general" $ do
        it "parses a general section in the expected order" $ do
            general `shouldParse` "[General]\r\nAudioFilename: test.mp3\r\nAudioHash: 12345678\r\n" `as` V.General
                { V.audioFileName = Just "test.mp3"
                , V.audioHash     = Just "12345678" }
        it "parses a general section the the unexpected order" $ do
            general `shouldParse` "[General]\r\nAudioHash: 12345678\r\nAudioFilename: test.mp3\r\n" `as` V.General
                { V.audioFileName = Just "test.mp3"
                , V.audioHash     = Just "12345678" }
        it "parses a metadata section in the expected order with all the fields filled" $ do
            metadata `shouldParse` "[Metadata]\r\nTitle:foo\r\nArtist:bar\r\nCreator:foobar\r\nVersion:barfoo" `as` V.Metadata
                { V.title   = Just "foo"
                , V.artist  = Just "bar"
                , V.creator = Just "foobar"
                , V.version = Just "barfoo" }
        it "parses a metadata section in the expected order with all but one field filled" $ do
            metadata `shouldParse` "[Metadata]\r\nTitle:foo\r\nArtist:bar\r\nCreator:foobar\r\nVersion:" `as` V.Metadata
                { V.title   = Just "foo"
                , V.artist  = Just "bar"
                , V.creator = Just "foobar"
                , V.version = Just "" }
    describe "timingPoint" $ do
        it "parses a valid timing point" $ do
            timingPoint `shouldParse` "2500,275.7" `as` V.TimingPoint
                { V.offset    = 2500
                , V.msPerBeat = 275.7}

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
    , V.timingPoints = [ V.TimingPoint { V.offset = 2000, V.msPerBeat = 173.8 } ]
    , V.hitObjects =
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
            { B.position = (100, 200)
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
                    { B.repeats = 1
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
                    { B.repeats = 3
                    , B.hitSoundsAndAdditions = [] }
                , B.pixelLength = 560 }
            , B.extras   = Nothing }
        , B.HitObject
            { B.position = (300, 50)
            , B.time     = 9000
            , B.newCombo = Nothing
            , B.hitSound = B.HitSound
                { B.normalHitSound  = False
                , B.whistleHitSound = False
                , B.finishHitSound  = False
                , B.clapHitSound    = False }
            , B.details = B.Spinner { B.endTime = 11000 }
            , B.extras   = Nothing } ] }

sampleBeatmapText =
    "[General]\
    \AudioFilename: Sample.mp3\
    \AudioHash: 12345678901234567890123456789012\
    \\
    \[Metadata]\
    \Title:foo\
    \Artist:bar\
    \Creator:foobar\
    \Version:\
    \\
    \[Difficulty]\
    \HPDrainRate:6\
    \CircleSize:4\
    \OverallDifficulty:6\
    \SliderMultiplier: 1.4\
    \SliderTickRate: 1\
    \\
    \[Events]\
    \foobar, because this isn't supported yet\
    \\
    \[TimingPoints]\
    \2000,173.8\
    \\
    \[HitObjects]\
    \100,200,2500,1,1,\
    \50,200,3000,2,2,B|32:192|32:384|480:384|480:160,3,560\
    \40,150,5000,6,4,C|160:160|128:32|384:32|320:192,3,560\
    \250,100,7000,2,8,L|320:96|162:95|160:322|352:320,1,560\
    \300,50,9000,12,0,11000"
