{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.BalsaSpec where

import qualified Context                              as C
import qualified Language.SimpleGo.AST                as S
import qualified Language.SimpleGo.Balsa              as B
import qualified Language.SimpleGo.Balsa.Builtins     as B
import qualified Language.SimpleGo.Balsa.Declarations as D
import qualified ParseTree                            as PT

import           Test.Hspec

runExpr p = B.runTranslateT $ B.runExprT (B.primExp p) f
  where
    f Nothing = return $ PT.NoCmd
    f (Just a) = return $ PT.SinkCmd D.pos a

spec :: Spec
spec = do
  describe "balsaType" $ do
    it "should convert a struct" $ do
      let struct = S.Struct [(S.Id "b", S.TypeName (S.Id "bool"))]
      t <- B.runTranslateT $ B.typeDecl struct
      t `shouldBe` (Right $ D.Type $ PT.RecordType D.pos [PT.RecordElem D.pos "b" (PT.NameType D.pos "bool")] PT.NoType)


  describe "primExp" $ do
    it "should handle the integer literal" $ do
      t <- runExpr $ S.LitInt 1
      t `shouldBe` (Right $ PT.SinkCmd D.pos (PT.ValueExpr B.pos B.byte (PT.IntValue 1)))
    it "should handle a call" $ do
      t <- runExpr $ S.Call (S.Qual Nothing (S.Id "test")) [] Nothing
      t `shouldBe` (Right $ PT.SeqCmd D.pos [PT.CallCmd D.pos (PT.NameCallable D.pos "test") C.EmptyContext [], PT.NoCmd])

  describe "simpleExp" $ do
    it "should handle a send" $ do
      let s = S.Send (S.Prim (S.Qual Nothing (S.Id "foo"))) (S.Prim (S.Qual Nothing (S.Id "bar")))
      t <- B.runTranslateT $ B.simpleExp s
      t `shouldBe` (Right (PT.OutputCmd D.pos (PT.NameChan D.pos "foo") (PT.NameExpr D.pos "bar")))
