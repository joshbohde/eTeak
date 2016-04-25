{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.Transforms (
  replaceIota
  ) where

import           Control.Monad.Identity        (runIdentity)
import           Language.SimpleGo.AST.Generic

replaceIota :: Integer -> Expr a -> Expr a
replaceIota i = f
  where
    f :: Expr a -> Expr a
    f (Prim (Qual Nothing (Id "iota"))) = Prim (LitInt i)
    f x = x
