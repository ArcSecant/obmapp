{-# LANGUAGE OverloadedStrings #-}

module Obmapp.Parser where

import Data.List.NonEmpty (fromList)
import Data.Word (Word8)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Obmapp.Beatmap

type Parser = Parsec (ErrorItem T.Text) T.Text

nothing :: Parser ()
nothing = pure ()

symbol :: T.Text -> Parser T.Text
symbol = L.symbol nothing

linespace :: Parser ()
linespace = const () <$> many (oneOf [' ', '\t'])

untilNextLine :: Parser ()
untilNextLine = (\_ _ -> ()) <$> linespace <*> eol

word8 :: Parser Word8
word8 = do
    n <- nat
    if n >= 0 && n <= 255
        then pure . fromIntegral $ n
        else customFailure . Label . fromList $ "Out of range ([0, 255]): " ++ show n

nat :: Parser Int
nat = fromInteger <$> L.decimal

int :: Parser Int
int = L.signed nothing nat

float :: Parser Double
float = try (L.signed nothing L.float) <|> (fromIntegral <$> int)

lstOfInt :: Parser [Int]
lstOfInt = sepBy int $ char ','

lstOfStr :: Parser [T.Text]
lstOfStr = sepBy textRemainingOnLine $ char ','

gameMode :: Parser GameMode
gameMode = char '0' *> pure Osu
    <|> char '1' *> pure Taiko
    <|> char '2' *> pure CatchTheBeat
    <|> char '3' *> pure OsuMania

textRemainingOnLine :: Parser T.Text
textRemainingOnLine = T.strip . T.pack <$> many (noneOf ['\r', '\n'])
