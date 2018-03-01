{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V5 where

import Control.Monad (void)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Perm

import Obmapp.Beatmap (TimingPoint (..), Colour, HitObject)
import qualified Obmapp.Beatmap.V5 as B
import Obmapp.Parser
import Obmapp.Parser.Osu

beatmap :: Parser B.Beatmap
beatmap = makePermParser $ (\general' metadata' difficulty' _ timingPoints' colours' hitObjects' -> B.Beatmap
    { B.general      = general'
    , B.metadata     = metadata'
    , B.difficulty   = difficulty'
    , B.timingPoints = timingPoints'
    , B.colours      = colours'
    , B.hitObjects   = hitObjects' })
        <$$> try general
        <||> try metadata
        <||> try difficulty
        <||> try events
        <||> try timingPoints
        <||> try colours
        <||> try hitObjects

general :: Parser B.General
general = section "General" $ makePermParser $ B.General
    <$?> (Nothing, kvPair "AudioFilename"   textValue)
    <|?> (Nothing, kvPair "AudioLeadIn"     int)
    <|?> (Nothing, kvPair "PreviewTime"     int)
    <|?> (Nothing, kvPair "SampleSet"       textValue)

metadata :: Parser B.Metadata
metadata = section "Metadata" $ makePermParser $ B.Metadata
    <$?> (Nothing, kvPair "Title"   textValue)
    <|?> (Nothing, kvPair "Artist"  textValue)
    <|?> (Nothing, kvPair "Creator" textValue)
    <|?> (Nothing, kvPair "Version" textValue)

difficulty :: Parser B.Difficulty
difficulty = section "Difficulty" $ makePermParser $ B.Difficulty
    <$?> (Nothing, kvPair "HPDrainRate"       float)
    <|?> (Nothing, kvPair "CircleSize"        float)
    <|?> (Nothing, kvPair "OverallDifficulty" float)
    <|?> (Nothing, kvPair "SliderMultiplier"  float)
    <|?> (Nothing, kvPair "SliderTickRate"    float)

events :: Parser ()
events = section "Events" (void $ many (notChar '['))

timingPoints :: Parser [TimingPoint]
timingPoints = section "TimingPoints" (many (const <$> timingPoint <*> untilNextLine))

timingPoint :: Parser TimingPoint
timingPoint = (\offset' _ msPerBeat' _ meter' _ sampleType' _ sampleSetInt' -> TimingPoint
    { offset       = offset'
    , msPerBeat    = msPerBeat'
    , meter        = Just meter'
    , sampleType   = Just sampleType'
    , sampleSetInt = Just sampleSetInt'
    , volume       = Nothing
    , inherited    = Nothing
    , kiaiMode     = Nothing })
    <$> int <*> char ',' <*> float <*> char ',' <*> int <*> char ',' <*> int <*> char ',' <*> int

colours :: Parser (M.Map Int Colour)
colours = undefined

hitObjects :: Parser [HitObject]
hitObjects = section "HitObjects" (many (const <$> hitObject <*> untilNextLine))