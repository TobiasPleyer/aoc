#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package bytestring
-}

import System.Environment
import Data.Int
import Text.Printf
import qualified Data.ByteString.Lazy as BL

calc_captcha :: BL.ByteString -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> (Int64, Int64)
calc_captcha bs length step sum count index =
  if (index == length) then (sum, count)
  else
    let curr = bs `BL.index` index
        next = bs `BL.index` ((index+step) `mod` length)
    in
      if curr == next
      then
        calc_captcha bs length step (sum+(fromIntegral curr)) (count+1) (index+1)
      else
        calc_captcha bs length step sum count (index+1)

main = do
  bs <- BL.readFile =<< (Prelude.head <$> getArgs)
  let input = BL.init bs  -- remove the annoying newline at the end
  let l = BL.length input
  let (sum_value, digit_count) = calc_captcha input l (l `div` 2) 0 0 0
  print (sum_value - digit_count*48)  -- 48 is the ascii value of '0'
