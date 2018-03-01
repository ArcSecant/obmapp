{-# LANGUAGE MultiParamTypeClasses #-}

module Obmapp.Beatmap.V5 where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Obmapp.Beatmap as B

data Beatmap = Beatmap
    { general :: General
    , metadata :: Metadata
    , difficulty :: Difficulty
    , timingPoints :: [B.TimingPoint]
    , colours :: M.Map Int B.Colour
    , hitObjects :: [B.HitObject] }
    deriving (Eq, Show)

instance B.Beatmap Beatmap where
    formatVersion = const $ B.FormatVersion 4
    timingPoints = timingPoints
    colours = colours
    hitObjects = hitObjects

instance B.General Beatmap where
    audioFileName = audioFileName . general
    audioLeadIn = audioLeadIn . general
    previewTime = previewTime . general
    countdown = countdown . general
    sampleSet = sampleSet . general

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

data General = General
    { audioFileName   :: Maybe T.Text
    , audioLeadIn     :: Maybe Int
    , previewTime     :: Maybe Int
    , countdown       :: Maybe Bool
    , sampleSet       :: Maybe B.SampleSet }
    deriving (Eq, Show)

data Metadata = Metadata
    { title   :: Maybe T.Text
    , artist  :: Maybe T.Text
    , creator :: Maybe T.Text
    , version :: Maybe T.Text }
    deriving (Eq, Show)

data Difficulty = Difficulty
    { hpDrainRate       :: Maybe Double
    , circleSize        :: Maybe Double
    , overallDifficulty :: Maybe Double
    , sliderMultiplier  :: Maybe Double
    , sliderTickRate    :: Maybe Double }
    deriving (Eq, Show)
