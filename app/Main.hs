{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Directory.Extra (listFilesRecursive)
import Text.Megaparsec

import Obmapp.Beatmap
import qualified Obmapp.Beatmap.General as G
import Obmapp.Parser.Beatmap
import Obmapp.Parser.Osu (versionInfo)

-- instance ShowToken T.Text where
--     showTokens = show

instance ShowErrorComponent (ErrorItem T.Text) where
    showErrorComponent = show

type FileResults = Maybe (Either (FormatVersion, ParseErrorBundle T.Text (ErrorItem T.Text)) G.Beatmap)

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
        Left _  -> Nothing
        Right v -> case parse beatmap path content of
            Left  e -> Just (Left  (v, e))
            Right b -> Just (Right b)

handleResults :: [FileResults] -> IO ()
handleResults results = do
    let (c, m) = countSummaries results
    putStrLn $ "Invalid version numbers: " ++ show c
    sequence_ $ map (\(FormatVersion v, (fails, oks)) -> putStrLn
        $ "Version " ++ show v ++ ": " ++ show oks ++ " OK, " ++ show fails ++ " failed")
        $ M.toList m
    sequence_ $ map (maybe (pure ()) id . printParseError) results

-- the count of files whose version numbers could not be parsed, as well as the
-- list of fail and success counts for files with correct version numbers
countSummaries :: [FileResults] -> (Int, M.Map FormatVersion (Int, Int))
countSummaries = mapSnd (M.map (\(Sum x, Sum y) -> (x, y))) . foldr f (0, M.empty) where
    f (Just (Right b))      = mapSnd $ M.insertWith (<>) (formatVersion b) (Sum 0, Sum 1)
    f (Just (Left  (v, _))) = mapSnd $ M.insertWith (<>) v                 (Sum 1, Sum 0)
    f Nothing               = mapFst (+ 1)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

firstError :: [FileResults] -> Maybe (ParseErrorBundle T.Text (ErrorItem T.Text))
firstError = foldr f Nothing where
    f (Just (Left (_, e))) Nothing = Just e
    f _                    _       = Nothing

printParseError :: FileResults -> Maybe (IO ())
printParseError r = case r of
    Just (Left (_, e)) -> Just $ putStrLn . errorBundlePretty $ e
    _                  -> Nothing