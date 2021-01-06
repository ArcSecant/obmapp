{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils where

import Data.Text.Internal as T
import qualified Text.Megaparsec as M

instance M.ShowErrorComponent (M.ErrorItem T.Text) where
    showErrorComponent = show