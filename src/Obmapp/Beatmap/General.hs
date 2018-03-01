module Obmapp.Beatmap.General where

import qualified Obmapp.Beatmap as B
import qualified Obmapp.Beatmap.V3 as V3
import qualified Obmapp.Beatmap.V4 as V4
import qualified Obmapp.Beatmap.V5 as V5

data Beatmap
    = BeatmapV3 V3.Beatmap
    | BeatmapV4 V4.Beatmap
    | BeatmapV5 V5.Beatmap
    deriving (Eq, Show)

instance B.Beatmap Beatmap where
    formatVersion b = B.FormatVersion $ case b of
        BeatmapV3 _ -> 3
        BeatmapV4 _ -> 4
        BeatmapV5 _ -> 5
    timingPoints b' = case b' of
        BeatmapV3 b -> V3.timingPoints b
        BeatmapV4 b -> V4.timingPoints b
        BeatmapV5 b -> V5.timingPoints b