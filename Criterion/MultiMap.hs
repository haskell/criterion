module Criterion.MultiMap
    (
      MultiMap
    , fromMap
    , toMap
    , singleton
    , lookup
    ) where

import Data.Monoid (Monoid(..))
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Set as S

newtype MultiMap k v = MultiMap {
      toMap :: M.Map k (S.Set v)
    }
    deriving (Eq, Ord, Read, Show)

instance (Ord k, Ord v) => Monoid (MultiMap k v) where
    mempty = MultiMap M.empty
    mappend (MultiMap a) (MultiMap b) = MultiMap (M.unionWith S.union a b)
    mconcat = MultiMap . M.unionsWith S.union . map toMap

fromMap :: M.Map k (S.Set v) -> MultiMap k v
fromMap = MultiMap

singleton :: k -> v -> MultiMap k v
singleton k v = MultiMap $ M.singleton k (S.singleton v)

lookup :: (Ord k, Ord v) => k -> MultiMap k v -> Maybe (S.Set v)
lookup k = M.lookup k . toMap
