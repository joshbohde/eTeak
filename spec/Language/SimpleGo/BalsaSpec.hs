{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.BalsaSpec where

import qualified Context                              as C
import qualified Language.SimpleGo.AST                as S
import qualified Language.SimpleGo.Balsa              as B
import qualified Language.SimpleGo.Balsa.Builtins     as B
import qualified Language.SimpleGo.Balsa.Declarations as D
import qualified ParseTree                            as PT

import           Data.Either                          (isLeft)
import           Test.Hspec

spec :: Spec
spec = do
  describe "balsaType" $
    it "should convert a struct" $ do
      let struct = S.Struct [(S.Id "b", S.TypeName (S.Id "bool"))]
      t <- B.runTranslateT $ B.typeDecl struct
      t `shouldBe` (Right $ D.Type $ PT.RecordType D.pos [PT.RecordElem D.pos "b" (PT.NameType D.pos "bool")] PT.NoType)


  describe "primExp" $ do
    let
      runExpr p = B.runTranslateT $ B.runExprCmd (B.primExp p) id
    it "should handle the integer literal" $ do
      (Right (B.ExprCmd _ (Just expr))) <- runExpr $ S.LitInt 1
      B.balsaExpr expr `shouldBe` PT.ValueExpr B.pos B.byte (PT.IntValue 1)

    it "should handle a call" $ do
      t <- runExpr $ S.Call (S.Qual Nothing (S.Id "test")) [] Nothing
      let
        cmd = PT.SeqCmd D.pos [PT.CallCmd D.pos (PT.NameCallable D.pos "test") C.EmptyContext [], PT.NoCmd]
      t `shouldBe` (Right $ B.ExprCmd (Just cmd) Nothing)

    it "should error on a type failure" $ do
      t <- runExpr $ S.Call (S.Qual Nothing (S.Id "print")) [S.Prim (S.Call (S.Qual Nothing (S.Id "test")) [] Nothing)] Nothing
      t `shouldSatisfy` isLeft


  describe "simpleExp" $
    it "should handle a send" $ do
      let s = S.Send (S.Prim (S.Qual Nothing (S.Id "foo"))) (S.Prim (S.Qual Nothing (S.Id "bar")))
      t <- B.runTranslateT $ B.simpleExp s
      let
        c = PT.OutputCmd D.pos (PT.NameChan D.pos "foo") (PT.NameExpr D.pos "bar")
      t `shouldBe` (Right $ B.ExprCmd (Just c) Nothing)
