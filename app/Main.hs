{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Directory.Extra (listFilesRecursive)
import Text.Megaparsec

import Obmapp.Beatmap (FormatVersion (..))
import Obmapp.Beatmap.General
import Obmapp.Parser.Beatmap
import Obmapp.Parser.Osu (versionInfo)

type FileResults = Maybe (Either FormatVersion Beatmap)

main :: IO ()
main = do
    files <- getArgs
        >>= fmap (filter (".osu" `isSuffixOf`) . concat)
         .  sequence
         .  map listFilesRecursive
    results <- sequence $ flip map files
         $  \file -> T.readFile file
        >>= \content -> pure $ handleFile file content
    handleResults results

handleFile :: FilePath -> T.Text -> FileResults
handleFile path content = do
    case parse versionInfo path content of
        Left _ -> Nothing
        Right version -> case parse beatmap path content of
            Left  _ -> Just (Left version)
            Right b -> Just (Right b)

handleResults :: [FileResults] -> IO ()
handleResults = flip (.) countFailsAndOKs $ \(fails, oks) -> do
    putStrLn $ "Successes: " ++ show oks
    putStrLn $ "Failures: "  ++ show fails

countFailsAndOKs :: [FileResults] -> (Int, Int)
countFailsAndOKs = foldr f (0, 0) where
    f (Just (Right _)) (fails, oks) = (fails,     oks + 1)
    f _                (fails, oks) = (fails + 1, oks)
