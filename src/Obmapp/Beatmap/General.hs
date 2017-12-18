module Obmapp.Beatmap.General where

import qualified Obmapp.Beatmap as B
import qualified Obmapp.Beatmap.V3 as V3

data Beatmap
    = BeatmapV3 V3.Beatmap
    deriving (Eq, Show)

instance B.Beatmap Beatmap where
    formatVersion b = B.FormatVersion $ case b of
        BeatmapV3 _ -> 3
    timingPoints (BeatmapV3 b) = V3.timingPoints b