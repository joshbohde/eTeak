{-# LANGUAGE MultiParamTypeClasses #-}

-- |

module Language.SimpleGo.Balsa.Unique where

class UniqueSupply m a where
  unique :: m a
