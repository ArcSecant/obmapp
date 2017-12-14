module Utils where

import qualified Data.Text as T
import Test.Hspec
import Obmapp.Parser

shouldParse = runParser
as r e = r `shouldBe` pure (e, T.empty)
asR r e = r `shouldBe` pure e
withError r e = cause' r `shouldBe` (Just e)

cause' :: Either ParseError (a, T.Text) -> Maybe Cause
cause' (Left (ParseError c _)) = Just c
cause' _ = Nothing
