module UnionFind
    ( UnionFind, size, fromList, union, values )
    where

import Prelude hiding (fromList, union)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M

data UnionFind k v = UnionFind
    { _keyToSet :: Map k Int
    , _setValue :: Map Int v
    }

size UnionFind{..} = M.size _setValue

fromList :: Ord k => [(k,v)] -> UnionFind k v
fromList kvs = UnionFind
    (M.fromList $ zip kk [0..])
    (M.fromList $ zip [0..] vv)
  where
    (kk,vv) = unzip kvs

find k UnionFind{..} = _setValue M.! (_keyToSet M.! k)

union a b uf@UnionFind{..}
    | aq == bq = uf
    | otherwise = UnionFind
        (M.map reset _keyToSet)
        (M.delete bq $ M.adjust (<>bv) aq _setValue)
  where
    aq = _keyToSet M.! a
    bq = _keyToSet M.! b
    bv = _setValue M.! bq
    reset q = if q == aq || q == bq then aq else q

values UnionFind{..} = M.elems _setValue
