module Lib where

import Data.Text as T

newtype Version = Version Int deriving (Eq, Show)

parseVersionInfo :: T.Text -> Maybe Version
parseVersionInfo = undefined
