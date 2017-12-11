module Obmapp.Parser.Beatmap.V3 where

import Obmapp.Beatmap.V3
import Obmapp.Parser

timingPoint :: Parser TimingPoint
timingPoint = (\offset' _ msPerBeat' -> TimingPoint
    { offset = offset'
    , msPerBeat = msPerBeat' })
    <$> int <*> char ',' <*> float
