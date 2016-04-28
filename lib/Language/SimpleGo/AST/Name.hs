{-# LANGUAGE FlexibleInstances #-}

-- |

module Language.SimpleGo.AST.Name where

import qualified Data.Text as T

-- | An opaque data structure representing a name in Go.
newtype Name = Name { unName :: T.Text } deriving (Eq, Show, Ord)

class HasName a where
  name :: a -> Name

instance HasName [Char] where
  {-# INLINE name #-}
  name = Name . T.pack

instance HasName T.Text where
  {-# INLINE name #-}
  name = Name
