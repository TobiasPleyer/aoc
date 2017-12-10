#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package array
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import System.Environment
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST


solve ::
  [Int] {- ^ The initial memory bank distribution -} ->
   Int  {- ^ Steps until loop back -}
solve banks = runST (do
  arr <- newListArray (0, length banks - 1) banks :: ST s (STArray s Int Int)
  loop [] arr
  )
  where
    loop :: [[Int]] -> STArray s Int Int -> ST s Int
    loop seen arr = do
      curr <- getElems arr
      if curr `elem` seen
      then
        do
          count_loop curr 1 arr
      else
        do
          distribute arr
          loop (curr:seen) arr
    count_loop t s a = do
      distribute a
      e <- getElems a
      if e == t
      then
        return s
      else
        do
          count_loop t (s+1) a
    distribute arr = do
      assocs <- getAssocs arr
      let (i,e) = findMax assocs
      let l = length assocs
      writeArray arr i 0
      one_by_one (i+1) e l arr
    findMax [] = error "Unexpected"
    findMax (a:as) = go a as
      where
        go a [] = a
        go (i,e) ((i',e'):as)
          | e' > e = go (i',e') as
          | otherwise = go (i,e) as
    one_by_one i e l arr
      | e <= 0 = return ()
      | otherwise = do
          let idx = i `mod` l
          val <- readArray arr idx
          writeArray arr idx (val+1)
          one_by_one (idx+1) (e-1) l arr

main = do
  content <- (readFile . head) =<< getArgs
  let mem_banks = map read (words content) :: [Int]
  print (solve mem_banks)
