{-# LANGUAGE OverloadedStrings #-}

-- |

module Language.SimpleGo.Eval (
  eval,
  Result(..),
  NumType(..),
  ) where

import qualified Data.Text               as T
import           Language.SimpleGo.AST
import           Language.SimpleGo.Types


data Result = IntegralR !NumType !Integer
            | FloatR !NumType !Float
            | CharR !Char
            | StringR !T.Text


eval :: Expr -> Maybe Result
eval (Prim p) = evalPrim p
eval Zero = Nothing
eval _ = Nothing

evalPrim :: Prim -> Maybe Result
evalPrim (LitInt i) = return $ IntegralR GoInt i
evalPrim (LitReal f) = return $ FloatR Float32 f
evalPrim (LitChar c) = return $ CharR c
evalPrim (LitStr t) = return $ StringR t
evalPrim (Call (Qual Nothing (Id "byte")) [e] _) = do
  a <- eval e
  case a of
    IntegralR _ i -> return $ IntegralR Uint8 i
    _ -> Nothing
evalPrim _ = Nothing
