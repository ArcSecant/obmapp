{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3 where

import Obmapp.Beatmap.V3
import Obmapp.Parser
import Obmapp.Parser.Osu

general :: Parser General
general = fmap (\(file, hash) -> General { audioFileName = file, audioHash = hash })
    $ section "General"
         $  kvPair "AudioFilename" textValue
        <?> kvPair "AudioHash"     textValue

metadata :: Parser Metadata
metadata = fmap (\(((title', artist'), creator'), version') -> Metadata
    { title = title'
    , artist = artist'
    , creator = creator'
    , version = version' })
    $ section "Metadata"
         $  kvPair "Title" textValue
        <?> kvPair "Artist" textValue
        <?> kvPair "Creator" textValue
        <?> kvPair "Version" textValue

timingPoint :: Parser TimingPoint
timingPoint = (\offset' _ msPerBeat' -> TimingPoint
    { offset = offset'
    , msPerBeat = msPerBeat' })
    <$> int <*> char ',' <*> float
