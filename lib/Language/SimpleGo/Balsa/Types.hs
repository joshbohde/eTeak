{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A module to declare and operate on both go and balsa types

module Language.SimpleGo.Balsa.Types (
  TypeDeclaration(..),
  TypeError(..),
  TypeNamespace(..),
  declareBuiltins,
  TypeM, runTypeM,
  translate, (=?)
  ) where

import           Prelude                          hiding (lookup)
import qualified Prelude

import           Control.Monad                    (forM_, unless)
import           Control.Monad.State              (StateT, evalStateT, get,
                                                   modify')
import           Control.Monad.Trans              (lift)
import qualified Data.Map                         as M

import qualified Language.SimpleGo.AST            as AST
import           Language.SimpleGo.AST.Name       (Name, name)
import qualified Language.SimpleGo.Balsa.Builtins as BalsaBuiltins
import qualified Language.SimpleGo.Types          as GoTypes
import qualified ParseTree                        as PT
import qualified Report                           as R

data TypeDeclaration = TypeDeclaration GoTypes.Type PT.TypeBody
                       deriving (Eq, Show)


data TypeError = Unfound Name
               | Incompatible GoTypes.Type GoTypes.Type
               | IncompatibleUntyped GoTypes.Type GoTypes.UnTyped
               | UnsupportedType AST.Type

class (Monad m) => TypeNamespace m where
  lookup :: Name -> m TypeDeclaration
  declare :: Name -> TypeDeclaration -> m ()
  typeError :: TypeError -> m a


declareBuiltins :: (TypeNamespace m) => m ()
declareBuiltins = forM_ GoTypes.primitives $ \(n, t) ->
    case Prelude.lookup n BalsaBuiltins.types of
      (Just bt) -> declare (name n) $ TypeDeclaration t $ PT.AliasType R.NoPos bt
      Nothing -> return ()

translate :: (TypeNamespace m) => AST.Type -> m GoTypes.Type
translate = go
  where
    goSig (AST.Signature ins outs) = GoTypes.Signature <$> traverse paramType ins <*> traverse paramType outs
    paramType (AST.Param _ t) = go t
    go (AST.TypeName (AST.Id i)) = do
      (TypeDeclaration t _) <- lookup (name i)
      return t
    -- only support static sizes at this point
    go (AST.ArrayType (AST.Prim (AST.LitInt i)) t) = GoTypes.Array i <$> go t
    go (AST.Channel k t) = GoTypes.Chan k <$> go t
    go (AST.FunctionType sig) = GoTypes.Func <$> goSig sig
    go (AST.MapType t t') = GoTypes.Map <$> go t <*> go t'
    go (AST.PointerType t) = GoTypes.Ptr <$> go t
    go (AST.SliceType t) = GoTypes.Slice <$> go t
    go (AST.Struct fields) = GoTypes.Struct <$> traverse (traverse go) fields
    go t = typeError $ UnsupportedType t


(=?) :: (TypeNamespace m) => AST.Type -> Either GoTypes.UnTyped AST.Type -> m ()
lhs =? rhs = do
  lht <- translate lhs
  case rhs of
    Left untype -> unless (GoTypes.canTypeAs untype lht) $
      typeError $ IncompatibleUntyped lht untype
    Right r -> do
      rht <- translate r
      unless (GoTypes.assignableTo lht rht) $
        typeError $ Incompatible lht rht

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
