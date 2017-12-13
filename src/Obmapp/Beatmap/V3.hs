{-# LANGUAGE MultiParamTypeClasses #-}

module Obmapp.Beatmap.V3 where

import qualified Data.Text as T
import qualified Obmapp.Beatmap as B

data Beatmap = Beatmap
    { general :: General
    , metadata :: Metadata
    , difficulty :: Difficulty
    , timingPoints :: [TimingPoint]
    , hitObjects :: [B.HitObject] }
    deriving (Eq, Show)

instance B.Beatmap Beatmap Beatmap Beatmap Beatmap TimingPoint Beatmap where
    formatVersion = const $ B.FormatVersion 3
    general = id
    editor = id
    metadata = id
    difficulty = id
    timingPoints = timingPoints
    hitObjects = hitObjects

instance B.General Beatmap where
    audioFileName = audioFileName . general
    audioHash = audioHash . general

instance B.Editor Beatmap where

instance B.Metadata Beatmap where
    title = title . metadata
    artist = artist . metadata
    creator = creator . metadata
    version = version . metadata

instance B.Difficulty Beatmap where
    hpDrainRate = hpDrainRate . difficulty
    circleSize = circleSize . difficulty
    overallDifficulty = overallDifficulty . difficulty
    sliderMultiplier = sliderMultiplier . difficulty
    sliderTickRate = sliderTickRate . difficulty

instance B.TimingPoint TimingPoint where
    offset       = offset
    msPerBeat    = msPerBeat
    meter        = const Nothing
    sampleType   = const Nothing
    sampleSetInt = const Nothing
    volume       = const Nothing
    inherited    = const Nothing
    kiaiMode     = const Nothing

data General = General
    { audioFileName :: Maybe T.Text
    , audioHash     :: Maybe T.Text }
    deriving (Eq, Show)

data Metadata = Metadata
    { title   :: Maybe T.Text
    , artist  :: Maybe T.Text
    , creator :: Maybe T.Text
    , version :: Maybe T.Text }
    deriving (Eq, Show)

data Difficulty = Difficulty
    { hpDrainRate       :: Maybe Float
    , circleSize        :: Maybe Float
    , overallDifficulty :: Maybe Float
    , sliderMultiplier  :: Maybe Float
    , sliderTickRate    :: Maybe Float }
    deriving (Eq, Show)

data TimingPoint = TimingPoint
    { offset    :: Int
    , msPerBeat :: Float }
    deriving (Eq, Show)
