module Obmapp.Parser.FormatError where

import Obmapp.Beatmap (FormatVersion)

data FormatError
    = UnsupportedVersion FormatVersion
    | MissingHitObjectType
    | UnknownSliderType Char
    | MismatchingSliderRepeats Int Int Int -- explicit repeat count, number of hit sounds, number of extras
    deriving (Eq, Show)
