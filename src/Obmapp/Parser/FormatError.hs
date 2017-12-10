module Obmapp.Parser.FormatError where

data FormatError
    = MissingHitObjectType
    | UnknownSliderType Char
    deriving (Eq, Show)
