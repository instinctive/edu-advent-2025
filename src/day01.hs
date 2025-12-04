#include "header.hs"

main = getContents >>= print . solve . parse

parse = lines >>> map \case
    (_:"0") -> 0
    ('L':n) -> read @Int n & negate
    ('R':n) -> read @Int n

solve xx =
    (part1,part2)
  where
    part1 = answer xx
    part2 = answer $ concatMap clicks xx

clicks 0 = [0]
clicks n = replicate (abs n) (signum n)

answer =
    length . filter (==0) .
    flip scanl 50 \z x -> mod (z+x) 100
