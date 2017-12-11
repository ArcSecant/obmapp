{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Text as T
import Test.Hspec

import qualified Obmapp.Beatmap as B
import Obmapp.Parser
import Obmapp.Parser.FormatError
import Obmapp.Parser.Osu

shouldParse = runParser
as r e = r `shouldBe` pure (e, T.empty)
asR r e = r `shouldBe` pure e
withError r e = r `shouldBe` Left [e]

main :: IO ()
main = hspec $ do
    describe "Obmapp.Parser.(<?>)" $ do
        it "parses an int and text in the expected order" $ do
            int <?> text "foo" `shouldParse` "42foo" `as` (42, "foo")
        it "parses an int and text in the unexpected order" $ do
            int <?> text "foo" `shouldParse` "foo42" `as` (42, "foo")
        it "parses an int, whitespace, and text in the expected order" $ do
            (int <?> whitespace <?> text "foo") `shouldParse` "foo 42" `as` ((42, " "), "foo")
        it "parses an int, whitespace, and text in an unexpected order" $ do
            (int <?> whitespace <?> text "foo") `shouldParse` " 42foo" `as` ((42, " "), "foo")
        it "parses an int, whitespace, and text in another unexpected order" $ do
            (int <?> whitespace <?> text "foo") `shouldParse` "42 foo" `as` ((42, " "), "foo")
    describe "Obmapp.Parser.between" $ do
        it "parses a specified char between | symbols" $ do
            between "|" "|" (char 'g') `shouldParse` "|g|" `as` 'g'
        it "parses specified text between brackets" $ do
            between "[" "]" (text "foobar") `shouldParse` "[foobar]" `as` "foobar"
        it "parses an int between parentheses" $ do
            between "(" ")" int `shouldParse` "(12357)" `as` 12357
    describe "Obmapp.Parser.int" $ do
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
        it "doesn't parse a word" $ do
            int `shouldParse` "foobar" `withError` ConditionNotFulfilled
    describe "Obmapp.Parser.float" $ do
        it "parses 0" $ do
            float `shouldParse` "0" `as` 0
        it "parses a positive int" $ do
            float `shouldParse` "1" `as` 1
        it "parses a negative int" $ do
            float `shouldParse` "-1" `as` (-1)
        it "parses a larger positive int" $ do
            float `shouldParse` "13" `as` 13
        it "parses a larger negative int" $ do
            float `shouldParse` "-17" `as` (-17)
        it "parses a positive decimal number" $ do
            float `shouldParse` "3.14" `as` 3.14 -- Float should be able to represent 3.14 accurately
        it "parses a negative decimal number" $ do
            float `shouldParse` "-4.2" `as` (-4.2)
        it "doesn't parse a word" $ do
            float `shouldParse` "foobar" `withError` ConditionNotFulfilled
    describe "Obmapp.Parser.text" $ do
        it "parses specified text" $ do
            text "foobar" `shouldParse` "foobar" `as` "foobar"
        it "parses specified text and returns the remaining text" $ do
            text "foo" `shouldParse` "foobar" `asR` ("foo", "bar")
    describe "Obmapp.Parser.while" $ do
        it "parses text consisting of digits" $ do
            while isDigit `shouldParse` "12357" `as` "12357"
        it "parses text consisting of spaces" $ do
            while (== ' ') `shouldParse` "   " `as` "   "
    describe "Obmapp.Parser.Osu.versionInfo" $ do
        it "parses version 1" $ do
            versionInfo `shouldParse` "osu file format v1" `as` B.FormatVersion 1
        it "parses a significantly newer version" $ do
            versionInfo `shouldParse` "osu file format v13" `as` B.FormatVersion 13
        it "doesn't parse version 0" $ do
            versionInfo `shouldParse` "osu file format v0" `withError` ConditionNotFulfilled
        it "doesn't parse a negative version" $ do
            versionInfo `shouldParse` "osu file format v-1" `withError` ConditionNotFulfilled
    describe "Obmapp.Parser.Osu.sectionTitle" $ do
        it "parses a non-empty section title" $ do
            sectionTitle `shouldParse` "[foobar]" `as` "foobar"
    describe "Obmapp.Parser.Osu.loneKeyValuePair" $ do
        it "parses a key-text pair" $ do
            loneKeyValuePair "foo" textValue `shouldParse` "foo: bar" `as` "bar"
        it "parses a key-text pair followed by a newline and more text" $ do
            loneKeyValuePair "foo" textValue `shouldParse` "foo: b\r\nar" `asR` ("b", "\r\nar")
        it "parses a key-int pair" $ do
            loneKeyValuePair "foo" int `shouldParse` "foo: 17" `as` 17
    describe "Obmapp.Parser.Osu.generalSectionV3" $ do
        it "parses a general section in the expected order" $ do
            generalSectionV3 `shouldParse` "[General]\r\nAudioFilename: test.mp3\r\nAudioHash: 12345678\r\n" `as` GeneralSectionV3 { audioFileName = Just "test.mp3", audioHash = Just "12345678" }
        it "parses a general section the the unexpected order" $ do
            generalSectionV3 `shouldParse` "[General]\r\nAudioHash: 12345678\r\nAudioFilename: test.mp3\r\n" `as` GeneralSectionV3 { audioFileName = Just "test.mp3", audioHash = Just "12345678" }
    describe "Obmapp.Parser.Osu.hitObjectDetails" $ do
        it "parses hit circle details" $ do
            hitObjectDetails HitCircle `shouldParse` "" `as` B.HitCircle
        it "parses slider details" $ do
            hitObjectDetails Slider `shouldParse` "L|320:240,1,12.5,1|2,0:0|1:2" `as` B.Slider
                { B.sliderShape = B.Linear (320, 240)
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
                , B.pixelLength = 12.5
                , B.edgeHitSounds = undefined
                , B.edgeAdditions = undefined }
        it "parses spinner details" $ do
            hitObjectDetails Spinner `shouldParse` "10" `as` B.Spinner { B.endTime = 10 }
    describe "Obmapp.Parser.Osu.hitObjectTypeDetails" $ do
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
    describe "Obmapp.Parser.Osu.hitSound" $ do
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
    describe "Obmapp.Parser.Osu.sliderShape" $ do
        it "parses a linear slider" $ do
            sliderShape `shouldParse` "L|320:240" `as`
                (B.Linear (320, 240))
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
    describe "Obmapp.Parser.Osu.breakWhen" $ do
        it "doesn't break when the condition is not fulfilled" $ do
            breakWhen (==) [1..5] `shouldBe` [[1..5]]
        it "breaks once in a simple equality case" $ do
            breakWhen (==) [1,2,2,3] `shouldBe` [[1, 2], [2, 3]]
    describe "Obmapp.Parser.Osu.sliderType" $ do
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
