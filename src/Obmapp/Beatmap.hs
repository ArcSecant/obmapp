module Obmapp.Beatmap where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Word (Word8)

class Beatmap b where
    formatVersion :: b -> FormatVersion

    timingPoints :: b -> [TimingPoint]

    -- events :: b -> [Events]

    colours :: b -> M.Map Int Colour
    colours = const M.empty

    hitObjects :: b -> [HitObject]
    hitObjects = const []

newtype FormatVersion = FormatVersion Int deriving (Eq, Show, Ord)

class General a where
    audioFileName :: a -> Maybe T.Text
    audioFileName = const Nothing

    audioHash :: a -> Maybe T.Text
    audioHash = const Nothing

    audioLeadIn :: a -> Maybe Int
    audioLeadIn = const Nothing

    previewTime :: a -> Maybe Int
    previewTime = const Nothing

    countdown :: a -> Maybe Bool
    countdown = const Nothing

    sampleSet :: a -> Maybe SampleSet
    sampleSet = const Nothing

    stackLeniency :: a -> Maybe Double
    stackLeniency = const Nothing

    mode :: a -> Maybe GameMode
    mode = const Nothing

    letterboxInBreaks :: a -> Maybe Bool
    letterboxInBreaks = const Nothing

    storyFireInFront :: a -> Maybe Bool
    storyFireInFront = const Nothing

    useSkinSprites :: a -> Maybe Bool
    useSkinSprites = const Nothing

    overlayPosition :: a -> Maybe T.Text
    overlayPosition = const Nothing

    skinPreference :: a -> Maybe T.Text
    skinPreference = const Nothing

    epilepsyWarning :: a -> Maybe Bool
    epilepsyWarning = const Nothing

    countdownOffset :: a -> Maybe Int
    countdownOffset = const Nothing

    specialStyle :: a -> Maybe Bool
    specialStyle = const Nothing

    widescreenStoryboard :: a -> Maybe Bool
    widescreenStoryboard = const Nothing

    samplesMatchPlaybackRate :: a -> Maybe Bool
    samplesMatchPlaybackRate = const Nothing

data GameMode
    = Osu
    | Taiko
    | CatchTheBeat
    | OsuMania
    deriving (Eq, Show)

type SampleSet = T.Text -- TODO: Figure out if this should be a sum type.

class Editor a where
    bookmarks :: a -> Maybe [Int]
    bookmarks = const Nothing

    distance :: a -> Maybe Double
    distance = const Nothing

    distanceSpacing :: a -> Maybe Double
    distanceSpacing = const Nothing

    beatDivisor :: a -> Maybe Double
    beatDivisor = const Nothing

    gridSize :: a -> Maybe Int
    gridSize = const Nothing

    timelineZoom :: a -> Maybe Double
    timelineZoom = const Nothing

class Metadata a where
    title :: a -> Maybe T.Text
    title = const Nothing

    titleUnicode :: a -> Maybe T.Text
    titleUnicode = const Nothing

    artist :: a -> Maybe T.Text
    artist = const Nothing

    artistUnicode :: a -> Maybe T.Text
    artistUnicode = const Nothing

    creator :: a -> Maybe T.Text
    creator = const Nothing

    version :: a -> Maybe T.Text
    version = const Nothing

    source :: a -> Maybe T.Text
    source = const Nothing

    tags :: a -> Maybe [T.Text]
    tags = const Nothing

    beatmapId :: a -> Maybe Int
    beatmapId = const Nothing

    beatmapSetId :: a -> Maybe Int
    beatmapSetId = const Nothing

class Difficulty a where
    hpDrainRate :: a -> Maybe Double
    hpDrainRate = const Nothing

    circleSize :: a -> Maybe Double
    circleSize = const Nothing

    overallDifficulty :: a -> Maybe Double
    overallDifficulty = const Nothing

    approachRate :: a -> Maybe Double
    approachRate = const Nothing

    sliderMultiplier :: a -> Maybe Double
    sliderMultiplier = const Nothing

    sliderTickRate :: a -> Maybe Double
    sliderTickRate = const Nothing

data Events = Events
    { eventType   :: T.Text
    , startTime   :: Int
    , eventParams :: [T.Text] }
    deriving (Eq, Show)

data TimingPoint = TimingPoint
    { offset       :: Double
    , beatLength   :: Double
    , meter        :: Maybe Int
    , sampleType   :: Maybe Int
    , sampleSetIdx :: Maybe Int
    , volume       :: Maybe Int
    , uninherited  :: Maybe Bool
    , kiaiMode     :: Maybe Int }
    deriving (Eq, Show)

type Colour = (Word8, Word8, Word8)

data HitObject = HitObject
    { position :: Point
    , time     :: Int
    , newCombo :: NewCombo
    , hitSound :: HitSound
    , details  :: HitObjectDetails
    , extras   :: Maybe HitObjectExtras }
    deriving (Eq, Show)

type Point = (Int, Int)

type NewCombo = Maybe Int -- how many combo colours to skip

data HitObjectDetails
    = HitCircle
    | Slider
        { sliderShape   :: SliderShape
        , edgeInfo      :: EdgeInfo
        , pixelLength   :: Double }
    | Spinner { endTime :: Int }
    deriving (Eq, Show)

data HitSound = HitSound
    { normalHitSound  :: Bool
    , whistleHitSound :: Bool
    , finishHitSound  :: Bool
    , clapHitSound    :: Bool }
    deriving (Eq, Show)

data SliderShape
    = Linear [Point]
    | Perfect Point Point
    | Bezier [[Point]]
    | Catmull [Point] -- deprecated, but who knows if it ever comes up
    deriving (Eq, Show)

data EdgeInfo = EdgeInfo
    { repeats :: Int -- redundant: should match up with the length of hitSoundsAndAdditions
    , hitSoundsAndAdditions :: [(HitSound, SliderExtras)]}
    deriving (Eq, Show)

data SliderExtras = SliderExtras
    { sliderSampleSet   :: Int
    , sliderAdditionSet :: Int }
    deriving (Eq, Show)

data HitObjectExtras = HitObjectExtras
    { extrasSampleSet    :: Int
    , extrasAdditionSet  :: Int
    , extrasCustomIndex  :: Int
    , extrasSampleVolume :: Int
    , extrasFileName     :: T.Text }
    deriving (Eq, Show)
