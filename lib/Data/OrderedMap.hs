-- |

module Data.OrderedMap where

import           Data.Function (on)
import           Data.List     (sortBy)
import qualified Data.Map      as M
import           Prelude       hiding (lookup)


data Map k v = Map Int (M.Map k (Int, v))
                      deriving (Eq, Show)

lookup :: (Ord k) => k -> Map k v -> Maybe v
lookup k (Map _ m) = snd <$> M.lookup k m

insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v (Map i m) = Map (succ i) (M.insert k (i, v) m)

toList :: Map k v -> [(k, v)]
toList (Map _ m) = map output $ sortBy (compare `on` inserted) $ M.toList m
  where
    output (k, (_,v)) = (k, v)
    inserted (_, (i, _)) = i

empty :: Map k v
empty = Map 0 M.empty
