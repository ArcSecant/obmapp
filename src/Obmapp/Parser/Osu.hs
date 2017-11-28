{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Osu where

import qualified Data.Text as T
import Obmapp.Parser

newtype Version = Version Int deriving (Eq, Show)

versionInfo :: Parser Version
versionInfo = const Version  <$> text "osu file format v" <*> resultFulfills (> 0) naturalNumber

sectionTitle :: Parser T.Text
sectionTitle = between "[" "]" (while (/= ']'))

keyValuePair :: T.Text -> Parser a -> Parser a
keyValuePair t p = (\_ _ _ _ x -> x)
    <$> text t
    <*> optional linespace
    <*> char ':'
    <*> optional linespace
    <*> p

textValue :: Parser T.Text
textValue = fmap T.strip $ untilT "\r\n"
