{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3 where

import Obmapp.Beatmap.V3
import Obmapp.Parser
import Obmapp.Parser.Osu

general :: Parser General
general = Parser $ \t -> do
    ((file, hash), t') <- flip runParser t $ section "General" (kvPair "AudioFilename" textValue <?> kvPair "AudioHash" textValue)
    pure $ (General { audioFileName = file, audioHash = hash }, t')

timingPoint :: Parser TimingPoint
timingPoint = (\offset' _ msPerBeat' -> TimingPoint
    { offset = offset'
    , msPerBeat = msPerBeat' })
    <$> int <*> char ',' <*> float
