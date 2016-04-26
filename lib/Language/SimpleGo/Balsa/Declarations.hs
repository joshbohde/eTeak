{-# LANGUAGE FlexibleContexts #-}
-- |

module Language.SimpleGo.Balsa.Declarations (
  Binding, Context,
  typeBinding,
  buildBindings, buildContext,
  Decl(..), declContext, alias, pos
  ) where

import           Control.Monad.Except (MonadError)
import           Control.Monad (zipWithM)
import qualified Data.Foldable as F
import qualified Data.Text as T

import qualified Context   as C
import qualified ParseTree as PT
import qualified Report    as R
import qualified Language.SimpleGo.AST as AST

type Binding = C.Binding PT.Decl
type Context = C.Context PT.Decl

data Decl = Type PT.TypeBody
          | Const AST.Type PT.Expr
          | Var AST.Type PT.Type (Maybe PT.Expr)
          | Chan AST.Type PT.Type
          | Proc AST.Type Context PT.Cmd
          -- For parameters
          | In AST.Type PT.Type
          | Out AST.Type PT.Type
          | Param AST.Type PT.Type
          deriving (Show, Eq)

alias :: PT.Type -> Decl
alias = Type . PT.AliasType pos

pos :: R.Pos
pos = R.NoPos

typeBinding :: Int -> String -> PT.Type -> Binding
typeBinding i name typ' = C.Binding i name C.TypeNamespace R.Incomplete $ PT.TypeDecl pos $ PT.AliasType pos typ'


buildBindings :: (MonadError String m, Foldable f) =>
                (Int -> a -> m Binding) -> f a -> m [Binding]
buildBindings mb as = zipWithM mb [0..] $ F.toList as

buildContext :: (MonadError String m, Foldable f) =>
              (Int -> a -> m Binding) -> f a -> m Context
buildContext mb as = C.bindingsToContext1 <$> buildBindings mb as

declContext :: [(T.Text, Decl)] -> Context
declContext decls = C.bindingsToContext1 $ zipWith binding [0..] decls
  where
    b (Type t) = (C.TypeNamespace, PT.TypeDecl pos t)
    b (Const _ e) = (C.OtherNamespace, PT.ExprDecl pos e)
    b (Var _ _ (Just e)) = (C.OtherNamespace, PT.ExprDecl pos e)
    b (Var _ t Nothing) = (C.OtherNamespace, PT.VarDecl pos t)
    b (Chan _ t) = (C.OtherNamespace, PT.ChanDecl pos t)
    b (Proc _ c cmd) = (C.ProcNamespace, PT.ProcDecl pos c [] cmd)
    -- For parameters
    b (In _ t) = (C.OtherNamespace, PT.PortDecl pos PT.Input t)
    b (Out _ t) = (C.OtherNamespace, PT.PortDecl pos PT.Output t)
    b (Param _ t) = (C.OtherNamespace, PT.ParamDecl pos True t)
    binding i (n, a) = C.Binding i (T.unpack n) namespace R.Incomplete decl
      where
        (namespace, decl) = b a
