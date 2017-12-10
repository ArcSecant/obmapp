module Obmapp.Parser.FormatError where

data FormatError
    = MissingHitObjectType
    | UnknownSliderType Char
    | MismatchingSliderRepeats Int Int Int -- explicit repeat count, number of hit sounds, number of extras
    deriving (Eq, Show)
