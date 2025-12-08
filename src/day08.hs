{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DerivingStrategies #-}

#include "header.hs"

import Control.Lens
import Linear.V3

newtype Box     = Box     Int deriving (Eq,Ord)          deriving newtype Show
newtype Size    = Size    Int deriving (Eq,Ord,Enum,Num) deriving newtype Show
newtype Circuit = Circuit Int deriving (Eq,Ord,Enum)     deriving newtype Show

data St = St
    { _nextCircuit  :: Circuit
    , _circuitSizes :: Map Circuit Size
    , _boxToCircuit :: Map Box Circuit
    } deriving Show
makeLenses ''St

newCircuit a b = do
    c <- nextCircuit <<%= succ
    circuitSizes . at c ?= 2
    boxToCircuit . at a ?= c
    boxToCircuit . at b ?= c

addToCircuit c a = do
    circuitSizes . ix c %= succ
    boxToCircuit . at a ?= c

mergeCircuits c d = do
    dsize <- use $ circuitSizes . at d . non 0
    circuitSizes . ix c += dsize
    circuitSizes . at d .= Nothing
    let rewire q = if q == c || q == d then c else q
    boxToCircuit . mapped %= rewire

connectBoxes (a,b) = do
    ac <- use $ boxToCircuit . at a
    bc <- use $ boxToCircuit . at b
    case (ac,bc) of
      (Nothing,Nothing) -> newCircuit a b
      (Nothing, Just c) -> addToCircuit c a
      (Just c, Nothing) -> addToCircuit c b
      (Just c,  Just d) | c == d -> pure ()
      (Just c,  Just d) -> mergeCircuits c d

solve q vv =
    (part1,part2)
  where
    n = length vv
    ary = listArray (1,n) vv
    -- box pairs sorted by distance
    dist2 a b = sum $ ((ary!a) - (ary!b))^2
    pairs = snd <$> sort [ (dist2 a b, (Box a, Box b)) | a <- [1..n-1], b <- [a+1..n] ]
    -- circuit candidates from successive connections
    cands = scanl go start pairs
    go st pair = flip execState st $ connectBoxes pair
    start = St (Circuit 0) M.empty M.empty
    -- compute the answers from the circuit candidates
    Size part1 = product $ take 3 $ reverse $ sort $ M.elems $ _circuitSizes $ cands !! q
    V3 part2 _ _ = (ary!a) * (ary!b)
    Just ((Box a, Box b),_) = find ((==n).M.size._boxToCircuit.snd) $ zip pairs $ tail cands

main = do
    [q] <- getArgs -- number of connections for part 1
    getContents >>= print . solve (readInt q) . parse

parse = lines >>> map (mk . readInts . splitOn ",") where mk [a,b,c] = V3 a b c
