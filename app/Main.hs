{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Megaparsec

import Obmapp.Beatmap.General
import Obmapp.Parser.Beatmap

main :: IO ()
main = getArgs >>= sequence_ . map ((TIO.putStrLn =<<) . handleFile)

handleFile :: FilePath -> IO T.Text
handleFile path = do
    content <- TIO.readFile path
    pure . summarize path $ parseMaybe beatmap content

summarize :: FilePath -> Maybe Beatmap -> T.Text
summarize path b = flip T.append (T.pack path) $ case b of
    Nothing -> "ERROR: "
    Just _  -> "SUCCESS: "
