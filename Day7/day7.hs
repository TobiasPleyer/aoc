#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package containers
-}

import System.Environment
import Data.Char
import qualified Data.List as L
import Text.ParserCombinators.ReadP
import qualified Data.Set as S
import qualified Data.Map.Strict as M


parse_identifier = many1 (satisfy isAlpha)

tower_line = do
  identifier <- parse_identifier
  skipSpaces
  weight <- between (char '(') (char ')') (many1 (satisfy isDigit))
  children <- getChildren <++ (return [])
  eof
  return (identifier, (read weight :: Int, children, 0::Int))

getChildren = do
  skipSpaces
  string "->"
  skipSpaces
  sepBy1 parse_identifier (string ", ")

parseTowers = map (fst . head . readP_to_S tower_line) . lines

findRoot :: [(String, (Int, [String], Int))] -> String
findRoot ts = (head . S.toList) (S.difference s1 s2)
  where
    (s1,s2) = L.foldl' (\(s1,s2) (n,(w,cs,t)) -> (S.insert n s1, S.union s2 (S.fromList cs))) (S.empty,S.empty) ts

main = do
  content <- (readFile . head) =<< getArgs
  let towers = parseTowers content
  putStrLn $ "root: " ++ findRoot towers
