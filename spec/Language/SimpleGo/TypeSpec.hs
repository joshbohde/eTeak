{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.TypeSpec where

--import qualified Language.SimpleGo.AST   as S
import           Language.SimpleGo.Types

import           Test.Hspec

spec :: Spec
spec = do
  describe "canTypeAs" $ do
    it "should be able to type an untyped int as an int" $ do
      canTypeAs DefaultInt (Numeric GoInt) `shouldBe` True

    it "should be able to type an untyped int as an named int" $ do
      canTypeAs DefaultInt (Named "myint" (Numeric GoInt)) `shouldBe` True
