#include "header.hs"

main = getContents >>= print . solve . parse

parse = transpose . map words . reverse . lines

solve probs =
    sum $ map app probs
  where
    app (f:xx) = op f $ readInts xx
    op "*" = product
    op "+" = sum
