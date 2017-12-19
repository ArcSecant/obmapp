module Utils where

import Data.Text.Internal as T
import qualified Text.Megaparsec as M

instance M.ShowToken T.Text where
    showTokens = show
