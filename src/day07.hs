#include "header.hs"

main = getContents >>= print . (solve1 &&& solve2) . parse

parse = lines

solve1 (l:ll) =
    flip execState 0 $
    foldM go start ll
  where
    start = [ bool c '|' $ c == 'S' | c <- l ]
    go (_:'|':_:aa) (_:'^':_:xx) =
        modify' succ >>
        ("|." <>) <$> go ('|':aa) ('?':xx)
    go (a:aa) (_:xx) =
        (a:) <$> go aa xx
    go _ _ = pure []

solve2 (l:ll) =
    ans
  where
    Just (_,ans) = find ((=='S').fst) $ zip l $ cands
    cands = foldr go (repeat 1) ll
    go qq nn =
        [ if b == '^' then x + z else y
        | ((_,x):(b,y):(_,z):_) <- tails $ zip (wrap '?' qq) (wrap 1 nn) ]

wrap q l = [q] <> l <> [q]
