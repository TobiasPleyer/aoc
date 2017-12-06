#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package bytestring
  --
  --
-}
{-# LANGUAGE FlexibleContexts #-}

import System.Environment
import Data.Int
import Text.ParserCombinators.ReadP
import qualified Data.ByteString.Char8 as BC


calc_checksum :: BC.ByteString -> Int
calc_checksum bs = sum diffs
  where lines = BC.lines bs
        numbers = map (BC.split '\t') lines
        values :: [[Int]]
        values = map (map (Prelude.read . BC.unpack)) numbers
        diffs = map minmax values
        minmax ns = maximum ns - minimum ns


main = do
  bs <- BC.readFile =<< (Prelude.head <$> getArgs)
  let input = BC.init bs  -- remove the annoying newline at the end
  let cs = calc_checksum input
  print cs
