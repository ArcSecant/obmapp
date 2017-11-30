{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser.Osu where

import qualified Data.Text as T
import Obmapp.Beatmap
import Obmapp.Parser

versionInfo :: Parser FormatVersion
versionInfo = const FormatVersion  <$> text "osu file format v" <*> resultFulfills (> 0) naturalNumber

data GeneralSectionV3 = GeneralSectionV3
    { audioFileName :: Maybe T.Text
    , audioHash :: Maybe T.Text }
    deriving (Eq, Show)

generalSectionV3 :: Parser GeneralSectionV3
generalSectionV3 = Parser $ \t -> do
    ((file, hash), t') <- flip runParser t $ section "General" (kvPair "AudioFilename" textValue <?> kvPair "AudioHash" textValue)
    pure $ (GeneralSectionV3 file hash, t')


section :: T.Text -> Parser a -> Parser a
section name p = Parser $ \t -> do
    (name', t') <- runParser (const <$> sectionTitle <*> whitespace) t
    if name == name'
        then runParser p t'
        else Left [MissingText $ T.unpack name]

sectionTitle :: Parser T.Text
sectionTitle = between "[" "]" (while (/= ']'))

kvPair :: T.Text -> Parser a -> Parser (Maybe a)
kvPair t p = const <$> optional (loneKeyValuePair t p) <*> whitespace

loneKeyValuePair :: T.Text -> Parser a -> Parser a
loneKeyValuePair t p = (\_ _ _ _ x -> x)
    <$> text t
    <*> optional linespace
    <*> char ':'
    <*> optional linespace
    <*> p

textValue :: Parser T.Text
textValue = fmap T.strip $ untilT "\r\n"
