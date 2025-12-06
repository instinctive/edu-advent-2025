#include "header.hs"

main = getContents >>= print . solve . parse

parse (splitOn [""] . lines -> [ranges,ids]) =
    ( map mkRange ranges, readInts ids )
  where
    mkRange (readInts . splitOn "-" -> [lo,hi]) = (lo,hi+1)

solve (ranges,ids) =
    (part1,part2)
  where
    part1 = length $ filter isFresh ids
    part2 = go 0 Nothing combined
    (ll,hh) = unzip ranges
    totals = M.fromListWith (+) $ ((,1) <$> ll) <> ((,-1) <$> hh)
    combined = scanl1 (\(_,x) (i,y) -> (i,x+y)) $ M.assocs totals
    m = M.fromList combined
    isFresh i = M.lookupLE i m & maybe False ((>0).snd)
    go !n Nothing [] = n
    go !n Nothing ((i,_):more) = go n (Just i) more
    go !n (Just i) ((j,0):more) = go (n+j-i) Nothing more
    go !n (Just i) (_:more) = go n (Just i) more
