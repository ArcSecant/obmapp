module Obmapp.Parser.Beatmap where

import Data.Set
import Text.Megaparsec

import qualified Obmapp.Beatmap as B
import Obmapp.Beatmap.General
import Obmapp.Parser
import Obmapp.Parser.Osu
import qualified Obmapp.Parser.Beatmap.V3 as V3

beatmap :: Parser Beatmap
beatmap = do
    B.FormatVersion v <- (\v' _ _ -> v') <$> versionInfo <*> untilNextLine <*> untilNextLine
    case v of
        3 -> BeatmapV3 <$> V3.beatmap
        _ -> failure Nothing empty