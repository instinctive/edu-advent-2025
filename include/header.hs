module Main where

import Prelude hiding ( readInt )

import Advent
import qualified UnionFind as UF

import Control.Monad.State
import Data.Array
import Data.List.Split (splitOn,chunksOf)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Linear.V2

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
