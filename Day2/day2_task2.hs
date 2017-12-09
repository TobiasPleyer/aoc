#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package text
-}
--{-# LANGUAGE FlexibleContexts #-}

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


even_diff :: T.Text -> Int
even_diff content =
  let ls = T.lines content
      nums :: [[Int]]
      nums = map (read . T.unpack) (map (T.split (== '\t')) ls)
  in calc_sum nums

calc_sum :: [[Int]] -> Int
calc_sum nums = sum (map head nums)

main = do
  t <- TIO.readFile =<< (head <$> getArgs)
  let content = T.init t
  let sum = even_diff content
  print sum
