{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
-- |

module Language.SimpleGo.Balsa.Expr (
  Expr(..)
  ) where

import qualified Language.SimpleGo.AST   as AST
import qualified Language.SimpleGo.Types as Types

data Expr a = Constant (Either Types.UnTyped Types.Type) a
            | Variable Types.Type a
            deriving (Eq, Show, Functor, Foldable, Traversable)
