#include "header.hs"

main = getContents >>= print . bimap solve solve . (parse1 &&& parse2)

parse1 = transpose . map words . reverse . lines

parse2 =
    map concat .
    splitOn [[]] .
    map words .
    transpose .
    quux .
    lines
  where
    -- move last line to first and add spaces after it
    quux ll = ops : map (const ' ') ops : reverse args
      where (ops:args) = reverse ll

solve probs =
    sum $ map app probs
  where
    app (f:xx) = op f $ readInts xx
    op "*" = product
    op "+" = sum
