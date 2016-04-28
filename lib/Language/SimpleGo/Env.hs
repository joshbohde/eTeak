-- |

module Language.SimpleGo.Env (
  Env, Error,
  lookup, insert,
  new,
  fresh, pop
  ) where

import qualified Data.OrderedMap as M
import qualified Data.Text       as T
import           Prelude         hiding (lookup)

-- Should really be a nonempty list here
-- A data type representing an environment of nested contexts

data Env a = Env [M.Map T.Text a] deriving (Eq, Show)

data Error = AlreadyDeclared
           | EmptyEnv
           deriving (Eq, Show)

-- Lookup a given entry in the environment
lookup :: Env a -> T.Text -> Maybe a
lookup (Env envs) k = go $ map (M.lookup k) envs
  where
    go [] = Nothing
    go (Just a:_) = Just a
    go (_:as) = go as


-- Insert a new entry into the environment
insert :: Env a -> T.Text -> a -> Either Error (Env a)
insert (Env []) _ _ = Left EmptyEnv
insert (Env (e:es)) k a= case M.lookup k e of
  Just _ -> Left AlreadyDeclared
  Nothing -> Right $ Env (M.insert k a e:es)

-- Add a new context to the environment
fresh :: Env a -> Env a
fresh (Env es) = Env (M.empty:es)

-- Remove the latests context
pop :: Env a -> Either Error ([(T.Text, a)], Env a)
pop (Env []) = Left EmptyEnv
pop (Env (m:ms)) = Right (M.toList m, Env ms)

-- An initial environment
new :: Env a
new = Env [M.empty]
