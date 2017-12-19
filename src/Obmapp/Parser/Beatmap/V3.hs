{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3 where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Perm

import Obmapp.Beatmap (TimingPoint (..), HitObject)
import qualified Obmapp.Beatmap.V3 as B
import Obmapp.Parser
import Obmapp.Parser.Osu

beatmap :: Parser B.Beatmap
beatmap = makePermParser $ (\general' metadata' difficulty' _ timingPoints' hitObjects' -> B.Beatmap
    { B.general      = general'
    , B.metadata     = metadata'
    , B.difficulty   = difficulty'
    , B.timingPoints = timingPoints'
    , B.hitObjects   = hitObjects' })
        <$$> general
        <||> metadata
        <||> difficulty
        <||> events
        <||> timingPoints
        <||> hitObjects

general :: Parser B.General
general = section "General" $ makePermParser $ B.General
    <$?> (Nothing, kvPair "AudioFilename" textValue)
    <|?> (Nothing, kvPair "AudioHash"     textValue)

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
timingPoint = (\offset' _ msPerBeat' -> TimingPoint
    { offset       = offset'
    , msPerBeat    = msPerBeat'
    , meter        = Nothing
    , sampleType   = Nothing
    , sampleSetInt = Nothing
    , volume       = Nothing
    , inherited    = Nothing
    , kiaiMode     = Nothing })
    <$> int <*> char ',' <*> float

hitObjects :: Parser [HitObject]
hitObjects = section "HitObjects" (many (const <$> hitObject <*> untilNextLine))