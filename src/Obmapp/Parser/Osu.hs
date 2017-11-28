{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Osu where

import Data.Char (isDigit)
import Data.Text as T
import Obmapp.Parser

newtype Version = Version Int deriving (Eq, Show)

versionInfo :: Parser Version
versionInfo = const Version  <$> text "osu file format v" <*> resultFulfills (> 0) naturalNumber
