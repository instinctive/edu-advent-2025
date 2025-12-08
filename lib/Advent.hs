module Advent where

import Prelude hiding (readInt)

readInt :: String -> Int
readInt = read

readInts :: [String] -> [Int]
readInts = map readInt
