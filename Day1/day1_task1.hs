#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package bytestring
-}

import System.Environment
import Data.Int
import qualified Data.ByteString.Lazy as BL

calc_captcha :: BL.ByteString -> Int64 -> Int64 -> Int64 -> Int64 -> (Int64, Int64)
calc_captcha bs l s c i =
  if (i == l) then (s, c)
  else
    let prev = bs `BL.index` i
        next = bs `BL.index` ((i+1) `mod` l)
    in
      if prev == next
      then
        calc_captcha bs l (s+(fromIntegral prev)) (c+1) (i+1)
      else
        calc_captcha bs l s c (i+1)

main = do
  bs <- BL.readFile =<< (Prelude.head <$> getArgs)
  let input = BL.init bs  -- remove the annoying newline at the end
  let l = BL.length input
  let (sum_value, digit_count) = calc_captcha input l 0 0 0
  print (sum_value - digit_count*48)  -- 48 is the ascii value of '0'
