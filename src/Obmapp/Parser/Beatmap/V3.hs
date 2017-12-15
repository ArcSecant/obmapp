{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Beatmap.V3 where

import Obmapp.Beatmap (HitObject)
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
    <?> timingPoints
    <?> hitObjects

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

timingPoints :: Parser [B.TimingPoint]
timingPoints = section "TimingPoints" (timingPoint `sepBy` whitespace) `withErrMsg` "Could not parse the timing points section."

timingPoint :: Parser B.TimingPoint
timingPoint = (\offset' _ msPerBeat' -> B.TimingPoint
    { B.offset = offset'
    , B.msPerBeat = msPerBeat' })
    <$> int <*> char ',' <*> float

hitObjects :: Parser [HitObject]
hitObjects = section "HitObjects" (hitObject `sepBy` whitespace) `withErrMsg` "Could not parse the hit objects section."