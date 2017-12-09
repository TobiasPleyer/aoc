#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package array
-}
{-# LANGUAGE BangPatterns #-}

import System.Environment
import Data.Array.MArray
import Data.Array.IO
import Text.Printf


do_jumps :: IOUArray Int Int -> Int -> Int -> Int -> (Int -> Int) -> Int -> IO Int
do_jumps arr !l !u !idx f !steps = do
  offset <- readArray arr idx               --find the next offset value
  let new_idx = idx + offset
  if (new_idx < l) || (new_idx > u)
  then return (steps + 1)                   --finished, return the required steps
  else do
    writeArray arr idx (f offset)           --increment the current offset value
    do_jumps arr l u new_idx f (steps+1)    --not finished, continue to jump


main = do
  content <- (readFile . head) =<< getArgs
  let jumps = map read $ words content :: [Int]
  let lower_bound = 0
  let upper_bound = length jumps - 1
  arr1 <- newListArray (lower_bound, upper_bound) jumps :: IO (IOUArray Int Int)
  task1 <- do_jumps arr1 0 (+1) 0
  arr2 <- newListArray (lower_bound, upper_bound) jumps :: IO (IOUArray Int Int)
  task2 <- do_jumps arr2 lower_bound upper_bound 0 (\o -> if o >= 3 then o-1 else o+1) 0
  printf "Solution 1st task: %d\n" task1
  printf "Solution 2nd task: %d\n" task2
