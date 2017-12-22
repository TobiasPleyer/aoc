#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package transformers
-}

import System.Environment
import Control.Monad.Trans.State.Lazy


data ParseState = PS
  { score :: !Int
  , nesting :: !Int
  , input :: String
  }

parseGroup :: Int -> Int -> Int -> [Char] -> (Int,Int)
parseGroup score garbage nesting []
  | nesting > 0 = error "Outermost group not closed"
  | otherwise = (score, garbage)
parseGroup score garbage nesting (c:cs) = case c of
  '{' -> parseGroup score garbage (nesting+1) cs
  '}' -> parseGroup (score+nesting) garbage (nesting-1) cs
  '<' -> parseGarbage score garbage nesting cs
  _   -> parseGroup score garbage nesting cs

parseGarbage :: Int -> Int -> Int -> [Char] -> (Int,Int)
parseGarbage score garbage nesting [] = error "Garbage at the end of input"
parseGarbage score garbage nesting (c:cs) = case c of
  '>' -> parseGroup score garbage nesting cs
  '!' -> parseGarbage score garbage nesting (tail cs)
  _   -> parseGarbage score (garbage+1) nesting cs

main = do
  content <- (readFile . head) =<< getArgs
  print $ parseGroup 0 0 0 content
