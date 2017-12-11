module Utils where

import qualified Data.Text as T
import Test.Hspec
import Obmapp.Parser

shouldParse = runParser
as r e = r `shouldBe` pure (e, T.empty)
asR r e = r `shouldBe` pure e
withError r e = r `shouldBe` Left [e]
