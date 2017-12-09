#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package bytestring
-}

import System.Environment
import Data.List (permutations, delete)
import Control.Monad (guard)

combinations xs = do
  x <- xs
  y <- delete x xs
  return (x,y)

not_anagrams (x,y)
  | length x /= length y = True
  | x == y = False
  | otherwise = null $ do
                        px <- permutations x
                        guard (px==y)
                        return (px,y)

counter b
  | b = 1
  | otherwise = 0

main = do
  content <- readFile =<< (head <$> getArgs)
  print $ sum $ map counter $ map ((all not_anagrams) . combinations . words) (lines content)
