#include "header.hs"

main = do
    [alg] <- getArgs
    let f = case alg of
            "std" -> standard
            "dyn" -> dynamic
    getContents >>= print . solve f . parse

parse = map (map digitToInt) . lines

solve f = sum . map (f 12)

-- sort to find the highest digit available per position
standard n xx =
    go 0 n ss
  where
    l = length xx
    ss = sortBy (comparing Down) $ zip xx [l,l-1..]
    go !z 0 _ = z
    go !z n ss =
        go (x + z * 10) (n-1) ss'
      where
        (aa,(x,i):bb) = span ((<n).snd) ss
        ss' = filter ((<i).snd) ss

-- build up a list of answers with dynamic programming
dynamic n =
    last . foldl' go (replicate n 0)
  where
    go answers x = zipWith max answers (0:answers & map \z -> x + z * 10)
