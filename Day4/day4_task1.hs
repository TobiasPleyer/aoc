#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package bytestring
-}

import System.Environment
import Data.List

combinations xs = do
  x <- xs
  y <- delete x xs
  return (x,y)

not_equal (x,y) = x /= y

counter b
  | b = 1
  | otherwise = 0

main = do
  content <- readFile =<< (head <$> getArgs)
  print $ sum $ map counter $ map ((all not_equal) . combinations . words) (lines content)
