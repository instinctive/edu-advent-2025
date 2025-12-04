#include "header.hs"

main = getContents >>= print . solve . parse

parse = splitOn "," >>> concatMap (expand . splitOn "-") where
    expand [lo,hi] = [read @Int lo .. read @Int hi]

solve cands =
    (part1,part2)
  where
    part1 = go invalid1
    part2 = go invalid2
    go f = sum $ filter f cands

invalid1 x = aa == bb where
    s = show x
    l = length s
    (aa,bb) = splitAt (div l 2) s

invalid2 x =
    (not (null qq) && all (==q) qq) ||
    any check [2..div l 2]
  where
    s@(q:qq) = show x
    l = length s
    check d = all (==q) qq where
        (q:qq) = chunksOf d s
