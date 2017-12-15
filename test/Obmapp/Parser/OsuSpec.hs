{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.OsuSpec where

import Test.Hspec

import Utils
import qualified Obmapp.Beatmap as B
import Obmapp.Parser
import Obmapp.Parser.FormatError
import Obmapp.Parser.Osu

spec :: Spec
spec = do
    describe "versionInfo" $ do
        it "parses version 1" $ do
            versionInfo `shouldParse` "osu file format v1" `as` B.FormatVersion 1
        it "parses a significantly newer version" $ do
            versionInfo `shouldParse` "osu file format v13" `as` B.FormatVersion 13
        it "doesn't parse version 0" $ do
            versionInfo `shouldParse` "osu file format v0" `withError` ConditionNotFulfilled
        it "doesn't parse a negative version" $ do
            versionInfo `shouldParse` "osu file format v-1" `withError` ConditionNotFulfilled
    describe "section" $ do
        it "parses a section containing a single int" $ do
            section "foo" int `shouldParse` "[foo]\r\n17" `as` 17
        it "parses a section containing leading whitespace and a single int" $ do
            section "foo" int `shouldParse` "    [foo]\r\n17" `as` 17
        it "parses a section containing trailing whitespace and a single int" $ do
            section "foo" int `shouldParse` "[foo]\r\n17    \r\n    " `as` 17
        it "doesn't parse a section with no whitespace between the title and content" $ do
            section "foo" int `shouldParse` "[foo]17" `withError` ConditionNotFulfilled
    describe "sectionTitle" $ do
        it "parses a non-empty section title" $ do
            sectionTitle `shouldParse` "[foobar]" `as` "foobar"
    describe "loneKeyValuePair" $ do
        it "parses a key-text pair" $ do
            loneKeyValuePair "foo" textValue `shouldParse` "foo: bar" `as` "bar"
        it "parses a key-text pair followed by a newline and more text" $ do
            loneKeyValuePair "foo" textValue `shouldParse` "foo: b\r\nar" `asR` ("b", "\r\nar")
        it "parses a key-int pair" $ do
            loneKeyValuePair "foo" int `shouldParse` "foo: 17" `as` 17
    describe "hitObject" $ do
        it "parses a hit circle with extras" $ do
            hitObject `shouldParse` "320,240,7500,1,1,0:0:0:0:" `as` B.HitObject
                { B.position = (320, 240)
                , B.time = 7500
                , B.newCombo = Nothing
                , B.hitSound = B.HitSound
                    { B.normalHitSound  = True
                    , B.whistleHitSound = False
                    , B.finishHitSound  = False
                    , B.clapHitSound    = False }
                , B.details = B.HitCircle
                , B.extras = Just B.HitObjectExtras
                    { B.extrasSampleSet    = 0
                    , B.extrasAdditionSet  = 0
                    , B.extrasCustomIndex  = 0
                    , B.extrasSampleVolume = 0
                    , B.extrasFileName     = "" } }
        it "parses a hit circle without extras" $ do
            hitObject `shouldParse` "100,200,2500,1,1," `as` B.HitObject
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
        it "parses a bezier slider with missing repeat extras and without other extras" $ do
            hitObject `shouldParse` "50,200,3000,2,2,B|32:192|32:384|480:384|480:160,3,560" `as` B.HitObject
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
        it "parses a catmull slider with missing repeat extras and without other extras" $ do
            hitObject `shouldParse` "40,150,5000,6,4,C|160:160|128:32|384:32|320:192,3,560" `as` B.HitObject
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
        it "parses a linear slider with missing repeat extras and without other extras" $ do
            hitObject `shouldParse` "250,100,7000,2,8,L|320:96|162:95|160:322|352:320,1,560" `as` B.HitObject
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
        it "parses a spinner without extras" $ do
            hitObject `shouldParse` "300,50,9000,12,0,11000" `as` B.HitObject
                { B.position = (300, 50)
                , B.time     = 9000
                , B.newCombo = Just 0
                , B.hitSound = B.HitSound
                    { B.normalHitSound  = False
                    , B.whistleHitSound = False
                    , B.finishHitSound  = False
                    , B.clapHitSound    = False }
                , B.details = B.Spinner { B.endTime = 11000 }
                , B.extras  = Nothing }
    describe "hitObjectDetails" $ do
        it "parses hit circle details" $ do
            hitObjectDetails HitCircle `shouldParse` "" `as` B.HitCircle
        it "parses slider details" $ do
            hitObjectDetails Slider `shouldParse` "L|320:240,1,12.5,1|2,0:0|1:2" `as` B.Slider
                { B.sliderShape = B.Linear [(320, 240)]
                , B.edgeInfo = B.EdgeInfo
                    { B.repeats = 1
                    , B.hitSoundsAndAdditions =
                        [ (B.HitSound
                            { B.normalHitSound  = True
                            , B.whistleHitSound = False
                            , B.finishHitSound  = False
                            , B.clapHitSound    = False }
                          , B.SliderExtras
                            { B.sliderSampleSet   = 0
                            , B.sliderAdditionSet = 0 })
                        , (B.HitSound
                            { B.normalHitSound  = False
                            , B.whistleHitSound = True
                            , B.finishHitSound  = False
                            , B.clapHitSound    = False }
                          , B.SliderExtras
                            { B.sliderSampleSet   = 1
                            , B.sliderAdditionSet = 2 }) ] }
                , B.pixelLength = 12.5 }
        it "parses spinner details" $ do
            hitObjectDetails Spinner `shouldParse` "10" `as` B.Spinner { B.endTime = 10 }
    describe "hitObjectTypeDetails" $ do
        it "parses hit circle type" $ do
            hitObjectTypeDetails `shouldParse` "1" `as` (HitCircle, Nothing)
        it "parses slider type" $ do
            hitObjectTypeDetails `shouldParse` "2" `as` (Slider, Nothing)
        it "parses spinner type" $ do
            hitObjectTypeDetails `shouldParse` "8" `as` (Spinner, Nothing)
        it "parses new combo with no combo colour skips" $ do
            hitObjectTypeDetails `shouldParse` "6" `as` (Slider, Just 0)
        it "parses new combo with one combo colour skip" $ do
            hitObjectTypeDetails `shouldParse` "22" `as` (Slider, Just 1)
        it "parses new combo with four combo colour skips" $ do
            hitObjectTypeDetails `shouldParse` "70" `as` (Slider, Just 4)
        it "parses new combo with five combo colour skips" $ do
            hitObjectTypeDetails `shouldParse` "86" `as` (Slider, Just 5)
    describe "hitSound" $ do
        it "parses a hit sound with no sounds set" $ do
            hitSound `shouldParse` "0" `as` B.HitSound
                { B.normalHitSound  = False
                , B.whistleHitSound = False
                , B.finishHitSound  = False
                , B.clapHitSound    = False }
        it "parses a hit sound with just the normal hit sound set" $ do
            hitSound `shouldParse` "1" `as` B.HitSound
                { B.normalHitSound  = True
                , B.whistleHitSound = False
                , B.finishHitSound  = False
                , B.clapHitSound    = False }
        it "parses a hit sound with just the normal hit sound set" $ do
            hitSound `shouldParse` "2" `as` B.HitSound
                { B.normalHitSound  = False
                , B.whistleHitSound = True
                , B.finishHitSound  = False
                , B.clapHitSound    = False }
        it "parses a hit sound with just the normal hit sound set" $ do
            hitSound `shouldParse` "4" `as` B.HitSound
                { B.normalHitSound  = False
                , B.whistleHitSound = False
                , B.finishHitSound  = True
                , B.clapHitSound    = False }
        it "parses a hit sound with just the normal hit sound set" $ do
            hitSound `shouldParse` "8" `as` B.HitSound
                { B.normalHitSound  = False
                , B.whistleHitSound = False
                , B.finishHitSound  = False
                , B.clapHitSound    = True }
        it "parses a hit sound with two hit sounds set" $ do
            hitSound `shouldParse` "10" `as` B.HitSound
                { B.normalHitSound  = False
                , B.whistleHitSound = True
                , B.finishHitSound  = False
                , B.clapHitSound    = True }
        it "parses a hit sound with all hit sounds set" $ do
            hitSound `shouldParse` "15" `as` B.HitSound
                { B.normalHitSound  = True
                , B.whistleHitSound = True
                , B.finishHitSound  = True
                , B.clapHitSound    = True }
    describe "sliderShape" $ do
        it "parses a linear slider" $ do
            sliderShape `shouldParse` "L|320:240" `as`
                (B.Linear [(320, 240)])
        it "parses a perfect slider" $ do
            sliderShape `shouldParse` "P|320:240|120:80" `as`
                (B.Perfect (320, 240) (120, 80))
        it "parses a one-piece bezier slider" $ do
            sliderShape `shouldParse` "B|320:240|120:80" `as`
                (B.Bezier [[(320, 240), (120, 80)]])
        it "parses a two-piece bezier slider" $ do
            sliderShape `shouldParse` "B|320:240|120:80|120:80|240:200" `as`
                (B.Bezier [[(320, 240), (120, 80)], [(120, 80), (240, 200)]])
        it "parses a three-piece bezier slider" $ do
            sliderShape `shouldParse` "B|320:240|120:80|120:80|240:200|240:200|170:150|80:140" `as`
                (B.Bezier
                    [ [(320, 240), (120, 80)]
                    , [(120, 80), (240, 200)]
                    , [(240, 200), (170, 150), (80, 140)] ])
        it "parses a catmull slider" $ do
            sliderShape `shouldParse` "C|320:240|120:80" `as`
                (B.Catmull[ (320, 240), (120, 80)])
    describe "breakWhen" $ do
        it "doesn't break when the condition is not fulfilled" $ do
            breakWhen (==) [1..5] `shouldBe` [[1..5]]
        it "breaks once in a simple equality case" $ do
            breakWhen (==) [1,2,2,3] `shouldBe` [[1, 2], [2, 3]]
    describe "sliderType" $ do
        it "parses the linear slider type symbol" $ do
            sliderType `shouldParse` "L" `as` Linear
        it "parses the perfect slider type symbol" $ do
            sliderType `shouldParse` "P" `as` Perfect
        it "parses the bezier slider type symbol" $ do
            sliderType `shouldParse` "B" `as` Bezier
        it "parses the catmull slider type symbol" $ do
            sliderType `shouldParse` "C" `as` Catmull
        it "doesn't parse an invalid slider type" $ do
            sliderType `shouldParse` "A" `withError` (FormatError $ UnknownSliderType 'A')
        it "doesn't parse a empty string" $ do
            sliderType `shouldParse` "" `withError` (EndOfInput)
