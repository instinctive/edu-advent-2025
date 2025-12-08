#include "header.hs"

import Linear.V3

main = do
    [q] <- getArgs -- number of connections for part 1
    getContents >>= print . solve (readInt q) . parse

parse = lines >>> map (mk . readInts . splitOn ",") where mk [a,b,c] = V3 a b c

solve q vv =
    (part1,part2)
  where
    n = length vv
    ary = listArray (1,n) vv
    -- box pairs sorted by distance
    dist2 a b = sum $ ((ary!a) - (ary!b))^2
    pairs = snd <$> sort [ (dist2 a b, (a,b)) | a <- [1..n-1], b <- [a+1..n] ]
    -- circuit candidates from successive connections
    cands = scanl go start pairs
    start = UF.fromList $ zip [1..n] (repeat $ Sum 1)
    go uf (a,b) = UF.union a b uf
    -- compute answers from the circuit candidates
    Sum part1 = product $ take 3 $ sortBy (comparing Down) $ UF.values $ cands !! q
    V3 part2 _ _ = (ary!a) * (ary!b) where
        Just ((a,b),_) = find ((==1).UF.size.snd) $ zip pairs $ tail cands
