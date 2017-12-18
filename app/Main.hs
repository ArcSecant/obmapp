{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Obmapp.Beatmap.General
import Obmapp.Parser
import Obmapp.Parser.Beatmap

main :: IO ()
main = getArgs >>= sequence_ . map ((TIO.putStrLn =<<) . handleFile)

handleFile :: FilePath -> IO T.Text
handleFile path = do
    content <- TIO.readFile path
    pure $ summarize path (runParser beatmap content)

summarize :: FilePath -> Either ParseError (Beatmap, T.Text) -> T.Text
summarize path (Left (ParseError cause' description')) =
    "ERROR: " `T.append` T.pack path `T.append` " | " `T.append` case description' of
        Nothing   -> T.pack (show cause')
        Just desc -> T.pack desc `T.append` " (" `T.append` T.pack (show cause') `T.append` ")"
summarize path (Right (_, t)) =
    "SUCCESS: " `T.append` T.pack path `T.append` " | Remaining: " `T.append` (T.pack . show . T.length $ t)
