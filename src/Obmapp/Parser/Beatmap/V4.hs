{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V4 where

import Control.Monad (void)
import Control.Applicative.Permutations
import Text.Megaparsec
import Text.Megaparsec.Char

import Obmapp.Beatmap (TimingPoint (..), HitObject)
import qualified Obmapp.Beatmap.V4 as B
import Obmapp.Parser
import Obmapp.Parser.Osu

beatmap :: Parser B.Beatmap
beatmap = runPermutation $ (\general' metadata' difficulty' _ timingPoints' hitObjects' -> B.Beatmap
    { B.general      = general'
    , B.metadata     = metadata'
    , B.difficulty   = difficulty'
    , B.timingPoints = timingPoints'
    , B.hitObjects   = hitObjects' })
        <$> toPermutation general
        <*> toPermutation metadata
        <*> toPermutation difficulty
        <*> toPermutation events
        <*> toPermutation timingPoints
        <*> toPermutation hitObjects

general :: Parser B.General
general = section "General" $ runPermutation $ B.General
    <$> toPermutationWithDefault Nothing (kvPair "AudioFilename"   textValue)
    <*> toPermutationWithDefault Nothing (kvPair "AudioHash"       textValue)
    <*> toPermutationWithDefault Nothing (kvPair "AudioLeadIn"     int)
    <*> toPermutationWithDefault Nothing (kvPair "PreviewTime"     int)
    <*> toPermutationWithDefault Nothing (kvPair "SampleSet"       textValue)
    <*> toPermutationWithDefault Nothing (kvPair "EditorBookmarks" (int `sepBy` char ','))

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
timingPoint = (\offset' _ msPerBeat' _ meter' _ sampleType' _ sampleSetInt' -> TimingPoint
    { offset       = offset'
    , beatLength   = msPerBeat'
    , meter        = Just meter'
    , sampleType   = Just sampleType'
    , sampleSetIdx = Just sampleSetInt'
    , volume       = Nothing
    , uninherited  = Nothing
    , kiaiMode     = Nothing })
    <$> float <*> char ',' <*> float <*> char ',' <*> int <*> char ',' <*> int <*> char ',' <*> int

hitObjects :: Parser [HitObject]
hitObjects = section "HitObjects" (many (const <$> hitObject <*> untilNextLine))