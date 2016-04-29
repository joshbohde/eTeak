{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.TypeSpec where

--import qualified Language.SimpleGo.AST   as S
import           Language.SimpleGo.Types

import           Test.Hspec

int :: Type
int = Numeric GoInt

myint :: Type
myint = Named "myint" int

uint8 :: Type
uint8 = Numeric Uint8

spec :: Spec
spec = do
  describe "assignableTo" $ do
    it "should be able to assign the same type to a type" $
      assignableTo int int `shouldBe` True

    it "should not be able to assign a type alias to type alias to it's underlying type" $
      assignableTo myint int `shouldBe` False

    it "should not be able to assign a type alias to it's underlying type" $
      assignableTo int myint `shouldBe` False

    it "should be able to assign uint8 to bytes" $
      assignableTo uint8 builtinByte `shouldBe` True

  describe "canTypeAs" $ do
    it "should be able to type an untyped int as an int" $
      canTypeAs DefaultInt int `shouldBe` True

    it "should be able to type an untyped int as an named int" $
      canTypeAs DefaultInt myint `shouldBe` True

    it "should be able to type an untyped int as an named int" $
      canTypeAs DefaultInt myint `shouldBe` True

    it "should be able to type an untyped int as a byte" $
      canTypeAs DefaultInt builtinByte `shouldBe` True
