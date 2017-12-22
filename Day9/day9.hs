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

parseGroup :: Int -> Int -> [Char] -> Int
parseGroup score nesting []
  | nesting > 0 = error "Outermost group not closed"
  | otherwise = score
parseGroup score nesting (c:cs) = case c of
  '{' -> parseGroup score (nesting+1) cs
  '}' -> parseGroup (score+nesting) (nesting-1) cs
  '<' -> parseGarbage score nesting cs
  _   -> parseGroup score nesting cs

parseGarbage :: Int -> Int -> [Char] -> Int
parseGarbage score nesting [] = error "Garbage at the end of input"
parseGarbage score nesting (c:cs) = case c of
  '>' -> parseGroup score nesting cs
  '!' -> parseGarbage score nesting (tail cs)
  _   -> parseGarbage score nesting cs

main = do
  content <- (readFile . head) =<< getArgs
  print $ parseGroup 0 0 content
