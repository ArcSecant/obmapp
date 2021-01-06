{-# LANGUAGE MultiParamTypeClasses #-}

module Obmapp.Beatmap.V14 where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Obmapp.Beatmap as B

data Beatmap = Beatmap
    { general :: General
    , editor :: Editor
    , metadata :: Metadata
    , difficulty :: Difficulty
    -- , events :: [B.Events]
    , timingPoints :: [B.TimingPoint]
    , colours :: M.Map Int B.Colour
    , hitObjects :: [B.HitObject] }
    deriving (Eq, Show)

instance B.Beatmap Beatmap where
    formatVersion = const $ B.FormatVersion 14
    timingPoints = timingPoints
    -- events = events
    colours = colours
    hitObjects = hitObjects

instance B.General Beatmap where
    audioFileName = audioFileName . general
    audioLeadIn = audioLeadIn . general
    previewTime = previewTime . general
    countdown = countdown . general
    sampleSet = sampleSet . general
    stackLeniency = stackLeniency . general
    mode = mode . general
    letterboxInBreaks = letterboxInBreaks . general
    useSkinSprites = useSkinSprites . general
    overlayPosition = overlayPosition . general
    skinPreference = skinPreference . general
    epilepsyWarning = epilepsyWarning . general
    countdownOffset = countdownOffset . general
    specialStyle = specialStyle . general
    widescreenStoryboard = widescreenStoryboard . general
    samplesMatchPlaybackRate = samplesMatchPlaybackRate . general

instance B.Editor Beatmap where
    bookmarks = bookmarks . editor
    distanceSpacing = distanceSpacing . editor
    beatDivisor = beatDivisor . editor
    gridSize = gridSize . editor
    timelineZoom = timelineZoom . editor

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
    { audioFileName     :: Maybe T.Text
    , audioLeadIn       :: Maybe Int
    , previewTime       :: Maybe Int
    , countdown         :: Maybe Bool
    , sampleSet         :: Maybe B.SampleSet
    , stackLeniency     :: Maybe Double
    , mode              :: Maybe B.GameMode
    , letterboxInBreaks :: Maybe Bool
    , useSkinSprites    :: Maybe Bool
    , overlayPosition   :: Maybe T.Text
    , skinPreference    :: Maybe T.Text
    , epilepsyWarning   :: Maybe Bool
    , countdownOffset   :: Maybe Int
    , specialStyle      :: Maybe Bool
    , widescreenStoryboard :: Maybe Bool
    , samplesMatchPlaybackRate :: Maybe Bool }
    deriving (Eq, Show)

data Editor = Editor
    { bookmarks :: Maybe [Int]
    , distanceSpacing :: Maybe Double
    , beatDivisor :: Maybe Double
    , gridSize :: Maybe Int
    , timelineZoom :: Maybe Double }
    deriving (Eq, Show)

data Metadata = Metadata
    { title   :: Maybe T.Text
    , titleUnicode :: Maybe T.Text
    , artist  :: Maybe T.Text
    , artistUnicode :: Maybe T.Text
    , creator :: Maybe T.Text
    , version :: Maybe T.Text
    , source :: Maybe T.Text
    , tags :: Maybe [T.Text]
    , beapmapID :: Maybe Int
    , beatmapSetID :: Maybe Int }
    deriving (Eq, Show)

data Difficulty = Difficulty
    { hpDrainRate       :: Maybe Double
    , circleSize        :: Maybe Double
    , overallDifficulty :: Maybe Double
    , approachRate      :: Maybe Double
    , sliderMultiplier  :: Maybe Double
    , sliderTickRate    :: Maybe Double }
    deriving (Eq, Show)
