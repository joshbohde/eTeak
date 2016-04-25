-- |

module Language.SimpleGo.AST (
  Program',
  Declaration',
  Rec',
  Signature',
  Param',
  Type',
  ChanKind',
  Expr',
  Prim',
  Value',
  Block',
  Statement',
  Simp',
  Chan',
  Cond',
  Case',
  G.Program(..),
  G.Declaration(..),
  G.Rec(..),
  G.Signature(..),
  G.Param(..),
  G.Type(..),
  G.ChanKind(..),
  G.Expr(..),
  G.Prim(..),
  G.Value(..),
  G.Block(..),
  G.Statement(..),
  G.Simp(..),
  G.Chan(..),
  G.Cond(..),
  G.Case(..),
  Fix(..),
  -- rexports
  G.Id(..),
  Operators.Unary(..),
  Operators.Binary(..)

  ) where

import           Data.Functor.Foldable           (Fix (..))
import qualified Data.Text                       as T
import qualified Data.Vector                     as U
import qualified Language.SimpleGo.AST.Generic   as G
import qualified Language.SimpleGo.AST.Operators as Operators


type Program' = G.Program (Fix G.Expr)
type Declaration' = G.Declaration Expr'
type Rec' = G.Rec Expr'
type Signature' = G.Signature Expr'
type Param' = G.Param Expr'
type Type' = G.Type Expr'
type ChanKind' = G.ChanKind
type Expr' = Fix G.Expr
type Prim' = G.Prim Expr'
type Value' = G.Value Expr'
type Block' = G.Block Expr'
type Statement' = G.Statement Expr'
type Simp' = G.Simp Expr'
type Chan' = G.Chan Expr'
type Cond' = G.Cond Expr'
type Case' f = G.Case f Expr'
