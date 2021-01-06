{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V5 where

import Control.Monad (void)
import Control.Applicative.Permutations
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Obmapp.Beatmap (TimingPoint (..), Colour, HitObject)
import qualified Obmapp.Beatmap.V5 as B
import Obmapp.Parser
import Obmapp.Parser.Osu

beatmap :: Parser B.Beatmap
beatmap = runPermutation $ (\general' metadata' difficulty' _ timingPoints' colours' hitObjects' -> B.Beatmap
    { B.general      = general'
    , B.metadata     = metadata'
    , B.difficulty   = difficulty'
    , B.timingPoints = timingPoints'
    , B.colours      = colours'
    , B.hitObjects   = hitObjects' })
        <$> toPermutation general
        <*> toPermutation metadata
        <*> toPermutation difficulty
        <*> toPermutation events
        <*> toPermutation timingPoints
        <*> toPermutation colours
        <*> toPermutation hitObjects

general :: Parser B.General
general = section "General" $ runPermutation $ B.General
    <$> toPermutationWithDefault Nothing (kvPair "AudioFilename"   textValue)
    <*> toPermutationWithDefault Nothing (kvPair "AudioLeadIn"     int)
    <*> toPermutationWithDefault Nothing (kvPair "PreviewTime"     int)
    <*> toPermutationWithDefault Nothing (kvPair "Countdown"       bool)
    <*> toPermutationWithDefault Nothing (kvPair "SampleSet"       textValue)

metadata :: Parser B.Metadata
metadata = section "Metadata" $ runPermutation $ B.Metadata
    <$> toPermutationWithDefault Nothing (kvPair "Title"   textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Artist"  textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Creator" textValue)
    <*> toPermutationWithDefault Nothing (kvPair "Version" textValue)

difficulty :: Parser B.Difficulty
difficulty = section "Difficulty" $ runPermutation $ B.Difficulty
    <$> toPermutationWithDefault Nothing (kvPair "HPDrainRate"       float)
    <*> toPermutationWithDefault Nothing (kvPair "CircleSize"        float)
    <*> toPermutationWithDefault Nothing (kvPair "OverallDifficulty" float)
    <*> toPermutationWithDefault Nothing (kvPair "SliderMultiplier"  float)
    <*> toPermutationWithDefault Nothing (kvPair "SliderTickRate"    float)

events :: Parser ()
events = section "Events" (void $ many (anySingleBut '['))

timingPoints :: Parser [TimingPoint]
timingPoints = section "TimingPoints" (many (const <$> timingPoint <*> untilNextLine))

timingPoint :: Parser TimingPoint
timingPoint = (\offset' _ msPerBeat' _ meter' _ sampleType' _ sampleSetInt' _ volume' -> TimingPoint
    { offset       = offset'
    , beatLength   = msPerBeat'
    , meter        = Just meter'
    , sampleType   = Just sampleType'
    , sampleSetIdx = Just sampleSetInt'
    , volume       = Just volume'
    , uninherited  = Nothing
    , kiaiMode     = Nothing })
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

colours :: Parser (M.Map Int Colour)
colours = section "Colours" colourValues

hitObjects :: Parser [HitObject]
hitObjects = section "HitObjects" (many (const <$> hitObject <*> untilNextLine))