{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3 where

import qualified Obmapp.Beatmap.V3 as B
import Obmapp.Parser
import Obmapp.Parser.Osu

beatmap :: Parser B.Beatmap
beatmap = fmap (\(((((general', metadata'), difficulty'), _), timingPoints'), hitObjects') -> B.Beatmap
    { B.general = general'
    , B.metadata = metadata'
    , B.difficulty = difficulty'
    , B.timingPoints = timingPoints'
    , B.hitObjects = hitObjects' })
     $  general
    <?> metadata
    <?> difficulty
    <?> events
    <?> atLeast 0 timingPoint `withErrMsg` "Could not parse the timing points section."
    <?> atLeast 0 hitObject `withErrMsg` "Could not parse the hit objects section."

general :: Parser B.General
general = flip withErrMsg "Could not parse the general section."
    $ fmap (\(file, hash) -> B.General { B.audioFileName = file, B.audioHash = hash })
    $ section "General"
         $  kvPair "AudioFilename" textValue
        <?> kvPair "AudioHash"     textValue

metadata :: Parser B.Metadata
metadata = flip withErrMsg "Could not parse the metadata section."
    $ fmap (\(((title', artist'), creator'), version') -> B.Metadata
    { B.title = title'
    , B.artist = artist'
    , B.creator = creator'
    , B.version = version' })
    $ section "Metadata"
         $  kvPair "Title" textValue
        <?> kvPair "Artist" textValue
        <?> kvPair "Creator" textValue
        <?> kvPair "Version" textValue

difficulty :: Parser B.Difficulty
difficulty = flip withErrMsg "Could not parse the difficulty section."
    $fmap (\((((hp, cs), od), sm), str) -> B.Difficulty
    { B.hpDrainRate       = hp
    , B.circleSize        = cs
    , B.overallDifficulty = od
    , B.sliderMultiplier  = sm
    , B.sliderTickRate    = str })
    $ section "Difficulty"
         $  kvPair "HPDrainRate"       float
        <?> kvPair "CircleSize"        float
        <?> kvPair "OverallDifficulty" float
        <?> kvPair "SliderMultiplier"  float
        <?> kvPair "SliderTickRate"    float

events :: Parser ()
events = flip withErrMsg "Could not parse the events section." $ const () <$> untilT "["

timingPoint :: Parser B.TimingPoint
timingPoint = (\offset' _ msPerBeat' -> B.TimingPoint
    { B.offset = offset'
    , B.msPerBeat = msPerBeat' })
    <$> int <*> char ',' <*> float
