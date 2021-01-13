module Obmapp.Beatmap.General where

import qualified Obmapp.Beatmap as B
import qualified Obmapp.Beatmap.V3 as V3
import qualified Obmapp.Beatmap.V4 as V4
import qualified Obmapp.Beatmap.V5 as V5
import qualified Obmapp.Beatmap.V14 as V14

data Beatmap
    = BeatmapV3 V3.Beatmap
    | BeatmapV4 V4.Beatmap
    | BeatmapV5 V5.Beatmap
    | BeatmapV14 V14.Beatmap
    deriving (Eq, Show)

instance B.Beatmap Beatmap where
    formatVersion b = B.FormatVersion $ case b of
        BeatmapV3 _ -> 3
        BeatmapV4 _ -> 4
        BeatmapV5 _ -> 5
        BeatmapV14 _ -> 14
    timingPoints b' = case b' of
        BeatmapV3 b -> V3.timingPoints b
        BeatmapV4 b -> V4.timingPoints b
        BeatmapV5 b -> V5.timingPoints b
        BeatmapV14 b -> V14.timingPoints b
    colours b' = case b' of
        BeatmapV3 b -> B.colours b
        BeatmapV4 b -> B.colours b
        BeatmapV5 b -> V5.colours b
        BeatmapV14 b -> V14.colours b
    hitObjects b' = case b' of
        BeatmapV3 b -> V3.hitObjects   b
        BeatmapV4 b -> V4.hitObjects   b
        BeatmapV5 b -> V5.hitObjects   b
        BeatmapV14 b -> V14.hitObjects b
    
instance B.General Beatmap where
    audioFileName b' = case b' of
        BeatmapV3 b -> V3.audioFileName   $ V3.general b
        BeatmapV4 b -> V4.audioFileName   $ V4.general b
        BeatmapV5 b -> V5.audioFileName   $ V5.general b
        BeatmapV14 b -> V14.audioFileName $ V14.general b

instance B.Difficulty Beatmap where
    sliderMultiplier b' = case b' of
        BeatmapV3 b -> V3.sliderMultiplier   $ V3.difficulty b
        BeatmapV4 b -> V4.sliderMultiplier   $ V4.difficulty b
        BeatmapV5 b -> V5.sliderMultiplier   $ V5.difficulty b
        BeatmapV14 b -> V14.sliderMultiplier $ V14.difficulty b