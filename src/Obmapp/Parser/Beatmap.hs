module Obmapp.Parser.Beatmap where

import qualified Obmapp.Beatmap as B
import Obmapp.Beatmap.General
import Obmapp.Parser
import Obmapp.Parser.FormatError
import Obmapp.Parser.Osu
import qualified Obmapp.Parser.Beatmap.V3 as V3

beatmap :: Parser Beatmap
beatmap = Parser $ \t -> do
    (fv@(B.FormatVersion v), t') <- runParser (const <$> versionInfo <*> whitespace) t
    flip runParser t' $ case v of
        3 -> BeatmapV3 <$> V3.beatmap
        _ -> Parser . const . Left $ ParseError
            { cause = FormatError . UnsupportedVersion $ fv
            , description = Nothing }