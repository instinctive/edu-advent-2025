#include "header.hs"

main = getContents >>= print . solve . parse

parse file =
    [ V2 r c
    | (r,line) <- zip [0..] (lines file)
    , (c,char) <- zip [0..] line
    , char == '@' ]

solve positions =
    ( part1, part2 )
  where
    part1 = head access
    part2 = sum access
    access = unfoldr go start
    start = foldr (M.adjust succ)
        ( M.fromList $ (,0) <$> positions )
        ( (+) <$> neighbors <*> positions )
    go m | M.null y = Nothing
         | otherwise = Just (M.size y, n')
      where
        (y,n) = M.partition (<4) m
        n' = foldr (M.adjust pred) n
            ( (+) <$> neighbors <*> M.keys y )

neighbors = V2 <$> [-1..1] <*> [-1..1] & delete 0
