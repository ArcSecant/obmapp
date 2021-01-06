{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V14 where

import Control.Monad (void)
import Control.Applicative.Permutations
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Obmapp.Beatmap (TimingPoint (..), Colour, HitObject)
import qualified Obmapp.Beatmap.V14 as B
import Obmapp.Parser
import Obmapp.Parser.Osu

beatmap :: Parser B.Beatmap
beatmap = runPermutation $ (\general' editor' metadata' difficulty' _' timingPoints' colours' hitObjects' -> B.Beatmap
    { B.general      = general'
    , B.editor       = editor'
    , B.metadata     = metadata'
    , B.difficulty   = difficulty'
    -- , B.events       = events'
    , B.timingPoints = timingPoints'
    , B.colours      = colours'
    , B.hitObjects   = hitObjects' })
        <$> toPermutation general
        <*> toPermutation editor
        <*> toPermutation metadata
        <*> toPermutation difficulty
        <*> toPermutation events
        <*> toPermutation timingPoints
        <*> toPermutation colours
        <*> toPermutation hitObjects

general :: Parser B.General
general = section "General" $ runPermutation $ B.General
    <$> toPermutationWithDefault Nothing (kvPair "AudioFilename"     textValue)
    <*> toPermutationWithDefault Nothing (kvPair "AudioLeadIn"       int)
    <*> toPermutationWithDefault Nothing (kvPair "PreviewTime"       int)
    <*> toPermutationWithDefault Nothing (kvPair "Countdown"         bool)
    <*> toPermutationWithDefault Nothing (kvPair "SampleSet"         textValue)
    <*> toPermutationWithDefault Nothing (kvPair "StackLeniency"     float)
    <*> toPermutationWithDefault Nothing (kvPair "Mode"              gameMode)
    <*> toPermutationWithDefault Nothing (kvPair "LetterboxInBreaks" bool)
    <*> toPermutationWithDefault Nothing (kvPair "UseSkinSprites"    bool)
    <*> toPermutationWithDefault Nothing (kvPair "OverlayPosition"   textValue)
    <*> toPermutationWithDefault Nothing (kvPair "SkinPreference"    textValue)
    <*> toPermutationWithDefault Nothing (kvPair "EpilepsyWarning"   bool)
    <*> toPermutationWithDefault Nothing (kvPair "CountdownOffset"   int)
    <*> toPermutationWithDefault Nothing (kvPair "SpecialStyle"      bool)
    <*> toPermutationWithDefault Nothing (kvPair "WidescreenStoryboard"     bool)
    <*> toPermutationWithDefault Nothing (kvPair "SamplesMatchPlaybackRate" bool)

editor :: Parser B.Editor
editor = section "Editor" $ runPermutation $ B.Editor
    <$> toPermutationWithDefault Nothing (kvPair "Bookmarks"        lstOfInt)
    <*> toPermutationWithDefault Nothing (kvPair "DistanceSpacing"  float)
    <*> toPermutationWithDefault Nothing (kvPair "BeatDivisor"      float)
    <*> toPermutationWithDefault Nothing (kvPair "GridSize"         int)
    <*> toPermutationWithDefault Nothing (kvPair "TimelineZoom"     float)


metadata :: Parser B.Metadata
metadata = section "Metadata" $ runPermutation $ B.Metadata
    <$> toPermutationWithDefault Nothing (kvPair "Title"         textValue)
    <*> toPermutationWithDefault Nothing (kvPair "TitleUnicode"  textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Artist"        textValue)
    <*> toPermutationWithDefault Nothing (kvPair "ArtistUnicode" textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Creator"       textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Version"       textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Source"        textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Tags"          lstOfStr)
    <*> toPermutationWithDefault Nothing (kvPair "BeatmapID"     int)
    <*> toPermutationWithDefault Nothing (kvPair "BeatmapSetID"  int)

difficulty :: Parser B.Difficulty
difficulty = section "Difficulty" $ runPermutation $ B.Difficulty
    <$> toPermutationWithDefault Nothing (kvPair "HPDrainRate"       float)
    <*> toPermutationWithDefault Nothing (kvPair "CircleSize"        float)
    <*> toPermutationWithDefault Nothing (kvPair "OverallDifficulty" float)
    <*> toPermutationWithDefault Nothing (kvPair "ApproachRate"      float)
    <*> toPermutationWithDefault Nothing (kvPair "SliderMultiplier"  float)
    <*> toPermutationWithDefault Nothing (kvPair "SliderTickRate"    float)

events :: Parser ()
events = section "Events" (void $ many (anySingleBut '['))

timingPoints :: Parser [TimingPoint]
timingPoints = section "TimingPoints" (many (const <$> timingPoint <*> untilNextLine))

timingPoint :: Parser TimingPoint
timingPoint = (\offset' _ msPerBeat' _ meter' _ sampleType' _ sampleSetInt' _ volume' _ uninherited' _ kiaiMode' -> TimingPoint
    { offset       = offset'
    , beatLength   = msPerBeat'
    , meter        = Just meter'
    , sampleType   = Just sampleType'
    , sampleSetIdx = Just sampleSetInt'
    , volume       = Just volume'
    , uninherited  = Just uninherited'
    , kiaiMode     = Just kiaiMode' })
    <$> float
    <*> char ','
    <*> float
    <*> char ','
    <*> int
    <*> char ','
    <*> int
    <*> char ','
    <*> int
    <*> char ','
    <*> int
    <*> char ','
    <*> bool
    <*> char ','
    <*> int

colours :: Parser (M.Map Int Colour)
colours = section "Colours" colourValues

hitObjects :: Parser [HitObject]
hitObjects = section "HitObjects" (many (const <$> hitObject <*> untilNextLine))