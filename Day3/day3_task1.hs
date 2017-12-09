#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package bytestring
-}


-- determines the square layer that a number lies on, 1 in the center
-- this number is what is needed to reach the number if it was to be found
-- on one of the coordinate axes
layer n = go n 1
  where
    go n s
      | n<=s*s = (s-1) `div` 2
      | otherwise = go n (s+2)

create_moves 0 = []
create_moves layer = cycle ((reverse l) ++ [0] ++ (init l))
  where l = [1..layer]

-- find the additional moves that are needed to reach the position
-- "moving along the other axis"
extra n = head $ drop ((square - n) `mod` width) moves
  where
    square = width*width
    width = l*2 + 1
    l = layer n
    moves = create_moves l

steps n = (layer n) + (extra n)

main = do
  let n = 312051
  print $ steps n
