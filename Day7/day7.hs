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


parseIdentifier = many1 (satisfy isAlpha)

parseTowerLine = do
  identifier <- parseIdentifier
  skipSpaces
  weight <- between (char '(') (char ')') (many1 (satisfy isDigit))
  children <- parseChildren <++ (return [])
  eof
  return (identifier, (read weight :: Int, children))

parseChildren = do
  skipSpaces
  string "->"
  skipSpaces
  sepBy1 parseIdentifier (string ", ")

parseTowers = map (fst . head . readP_to_S parseTowerLine) . lines

findRoot :: [(String, (Int, [String]))] -> String
findRoot ts = (head . S.toList) (S.difference s1 s2)
  where
    (s1,s2) = L.foldl' (\(s1,s2) (n,(w,cs)) -> (S.insert n s1, S.union s2 (S.fromList cs))) (S.empty,S.empty) ts

calculateWeight ::
  M.Map String (Int, [String]) {- The initial Map  -} ->
  String                       {- The root element -} ->
  Int                          {- The total weight -}
calculateWeight towerMap root = weight + (sum (map (calculateWeight towerMap) children))
  where
    Just (weight,children) = M.lookup root towerMap

main = do
  content <- (readFile . head) =<< getArgs
  let towers = parseTowers content
  let root = findRoot towers
  putStrLn $ "root: " ++ root
  let towerMap = M.fromList towers
  print $ calculateWeight towerMap root
