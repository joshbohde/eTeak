{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.Balsa.Declarations (
  Binding, Context,
  Decl(..),
  topLevelContext, subContext, pos
  ) where

import qualified Data.Text                     as T

import qualified Context                       as C
import qualified Language.SimpleGo.AST         as AST
import qualified Language.SimpleGo.Balsa.Types as Types
import           Language.SimpleGo.Types       (Type, UnTyped)
import qualified ParseTree                     as PT
import qualified Report                        as R

type Binding = C.Binding PT.Decl
type Context = C.Context PT.Decl

data Decl call = Type Types.TypeDeclaration
               | Const (Either UnTyped Type) PT.Expr
               | Var Type PT.Type (Maybe PT.Expr)
               | Chan Type PT.Type
               | Proc Type Context PT.Cmd call
               -- For parameters
               | In Type PT.Type
               | Out Type PT.Type
               | Param Type PT.Type
               deriving (Show, Eq)

pos :: R.Pos
pos = R.NoPos

class HasPTDecl a where
  mkDecl :: a -> (C.Namespace, PT.Decl)

instance HasPTDecl (Decl a) where
  mkDecl (Type (Types.TypeDeclaration _ t)) = (C.TypeNamespace, PT.TypeDecl pos t)
  mkDecl (Const _ e) = (C.OtherNamespace, PT.ExprDecl pos e)
  mkDecl (Var _ _ (Just e)) = (C.OtherNamespace, PT.ExprDecl pos e)
  mkDecl (Var _ t Nothing) = (C.OtherNamespace, PT.VarDecl pos t)
  mkDecl (Chan _ t) = (C.OtherNamespace, PT.ChanDecl pos t)
  mkDecl (Proc _ c cmd _) = (C.ProcNamespace, PT.ProcDecl pos c [] cmd)
  -- For parameters
  mkDecl (In _ t) = (C.OtherNamespace, PT.PortDecl pos PT.Input t)
  mkDecl (Out _ t) = (C.OtherNamespace, PT.PortDecl pos PT.Output t)
  mkDecl (Param _ t) = (C.OtherNamespace, PT.ParamDecl pos True t)

instance (HasPTDecl a, HasPTDecl b) => HasPTDecl (Either a b) where
  mkDecl (Right a) = mkDecl a
  mkDecl (Left a) = mkDecl a

data Builtin = Builtin C.Namespace PT.Decl

instance HasPTDecl Builtin where
  mkDecl (Builtin n d) = (n, d)

buildContext :: HasPTDecl a => [(T.Text, a)] -> Context
buildContext decls = C.bindingsToContext1 $ zipWith binding [0..] decls
  where
    binding i (n, a) = C.Binding i (T.unpack n) namespace R.Incomplete decl
      where
        (namespace, decl) = mkDecl a

topLevelContext :: [(T.Text, Decl a)] -> Context
topLevelContext decls = buildContext totalDecls
  where
    totalDecls = map (fmap Left) builtins ++ map (fmap Right) decls
    builtins = [
      ("String", Builtin C.TypeNamespace (PT.TypeDecl pos (PT.AliasType pos (PT.BuiltinType "String"))))
      ]

subContext :: [(T.Text, Decl a)] -> Context
subContext = buildContext
