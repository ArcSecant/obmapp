module Obmapp.Parser.Beatmap where

import Data.List.NonEmpty (fromList)
import Text.Megaparsec

import qualified Obmapp.Beatmap as B
import Obmapp.Beatmap.General
import Obmapp.Parser
import Obmapp.Parser.Osu
import qualified Obmapp.Parser.Beatmap.V3 as V3
import qualified Obmapp.Parser.Beatmap.V4 as V4
import qualified Obmapp.Parser.Beatmap.V5 as V5
import qualified Obmapp.Parser.Beatmap.V14 as V14

beatmap :: Parser Beatmap
beatmap = do
    B.FormatVersion v <- (\v' _ _ -> v') <$> versionInfo <*> untilNextLine <*> untilNextLine
    case v of
        3 -> BeatmapV3 <$> V3.beatmap
        4 -> BeatmapV4 <$> V4.beatmap
        5 -> BeatmapV5 <$> V5.beatmap
        14 -> BeatmapV14 <$> V14.beatmap
        _ -> customFailure . Label . fromList $ "Unsupported version: " ++ show v