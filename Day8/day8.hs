#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package containers
-}

import System.Environment
import qualified Data.Map.Strict as M


data Instruction = Inst
  { reg_name :: String
  , upd_func :: (Int -> Int)
  , cmp_reg :: String
  , cmp_func :: (Int -> Bool)
  }


mkInstruction (reg:op:val:_:creg:cmp:cval:[]) = inst
  where
    f x = case op of
      "dec" -> x - i
      "inc" -> x + i
      _ -> undefined
    c x = case cmp of
      "<" -> x < i2
      "<=" -> x <= i2
      ">" -> x > i2
      ">=" -> x >= i2
      "==" -> x == i2
      "!=" -> x /= i2
      _ -> undefined
    i = read val :: Int
    i2 = read cval :: Int
    inst = Inst {reg_name=reg, upd_func=f, cmp_reg=creg, cmp_func=c}
mkInstruction _ = Inst "x" (\x -> 0) "y" (\p -> False)

parse = map (mkInstruction . words) . lines

process m [] = calcMax m
process m (i:is) = process (exec_inst m i) is

exec_inst m i@(Inst reg ufunc creg cfunc) =
  let
    val = M.findWithDefault 0 reg m
    cval = M.findWithDefault 0 creg m
  in
    if (cfunc cval)
    then
      M.insert reg (ufunc val) m
    else
      m

calcMax = maximum . M.elems

main = do
  content <- (readFile . head) =<< getArgs
  print $ process M.empty (parse content)
