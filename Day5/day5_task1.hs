#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package array
-}

import System.Environment
import Data.Array.MArray
import Data.Array.IO


do_jumps :: IOUArray Int Int -> Int -> Int -> IO Int
do_jumps arr idx steps = do
  (l,u) <- getBounds arr
  offset <- readArray arr idx   --find the next offset value
  let new_idx = idx + offset
  if (new_idx < l) || (new_idx > u)
  then return (steps + 1)
  else do
    writeArray arr idx (offset+1) --increment the current offset value
    do_jumps arr new_idx (steps+1)

main = do
  content <- (readFile . head) =<< getArgs
  let jumps = map read $ words content :: [Int]
  let lower_bound = 0
  let upper_bound = length jumps - 1
  arr <- newListArray (lower_bound, upper_bound) jumps :: IO (IOUArray Int Int)
  steps <- do_jumps arr 0 0
  print steps
