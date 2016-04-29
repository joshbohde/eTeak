{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Language.SimpleGo.Balsa.TypesSpec where

import           Control.Monad.State           (StateT, evalStateT, get,
                                                modify')
import           Control.Monad.Trans           (lift)
import           Data.Either                   (isLeft)
import qualified Data.Map                      as M

import qualified Language.SimpleGo.AST         as AST
import           Language.SimpleGo.AST.Name    (Name, name)
import           Language.SimpleGo.Balsa.Types
import qualified Language.SimpleGo.Types       as Types

import           Test.Hspec

newtype TypeMap = TypeMap { unTypeMap :: M.Map Name TypeDeclaration }
type TypeM = StateT TypeMap (Either TypeError)

runTypeM :: TypeM a -> Either TypeError a
runTypeM s = evalStateT s (TypeMap M.empty)

instance TypeNamespace TypeM where
  lookup n = do
    (TypeMap m) <- get
    case M.lookup n m of
      Nothing -> lift $ Left (Unfound n)
      Just a -> return a
  declare n t = modify' (TypeMap . M.insert n t . unTypeMap)
  typeError = lift . Left

spec :: Spec
spec = do
  let
    prelude :: TypeM ()
    prelude = declareBuiltins

  describe "=?" $ do
    it "should be able to assign an int an untyped int" $ do
      let
        c = do
          prelude
          AST.TypeName (AST.Id "int") =? Left (Types.DefaultInt)
      runTypeM c `shouldBe` Right ()

    it "should be able to assign an int alias an untyped int" $ do
      let
        c = do
          prelude
          declareNamedType "myint" (AST.TypeName (AST.Id "int")) undefined
          AST.TypeName (AST.Id "myint") =? Left (Types.DefaultInt)
      runTypeM c `shouldBe` Right ()

    it "should not be able to assign an int alias an int" $ do
      let
        c = do
          prelude
          declareNamedType "myint" (AST.TypeName (AST.Id "int")) undefined
          AST.TypeName (AST.Id "myint") =? Right (AST.TypeName (AST.Id "int"))
      runTypeM c `shouldSatisfy` isLeft
