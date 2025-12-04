#include "header.hs"

main = getContents >>= print . solve . parse

parse = map (annotate . map digitToInt) . lines

annotate xx = zip xx [l,l-1..] where l = length xx

solve xxx =
    (part1,part2)
  where
    part1 = sum $ map (answer 0  2) xxx
    part2 = sum $ map (answer 0 12) xxx

answer !z 0 cc = z
answer !z n cc =
    answer (z*10+x) (n-1) cc'
  where
    ((x,_):cc') = foldr1 f (tails cc)
    f aa [] = aa
    f aa bb@((_,i):_)
        | i < n = aa
        | otherwise = max aa bb
