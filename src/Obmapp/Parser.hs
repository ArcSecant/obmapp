{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec (ErrorItem T.Text) T.Text

nothing :: Parser ()
nothing = pure ()

symbol :: T.Text -> Parser T.Text
symbol = L.symbol nothing

linespace :: Parser ()
linespace = const () <$> oneOf [' ', '\t']

untilNextLine :: Parser ()
untilNextLine = (\_ _ -> ()) <$> linespace <*> eol

nat :: Parser Int
nat = fromInteger <$> L.decimal

int :: Parser Int
int = L.signed nothing nat

float :: Parser Double
float = L.signed nothing L.float

textRemainingOnLine :: Parser T.Text
textRemainingOnLine = T.pack <$> many (const <$> anyChar <*> notFollowedBy eol) -- TODO: Add removing leading and trailing whitespace.
