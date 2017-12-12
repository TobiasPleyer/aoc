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

getChildren
  :: M.Map String (Int, [String])
  -> String
  -> [String]
getChildren m k = case (M.lookup k m) of
  Nothing -> []
  Just (w,cs) -> cs

getChildWeights towerMap root = map (calculateWeight towerMap) (getChildren towerMap root)

findDifference towerMap root = go $ getChildWeights towerMap root
  where
    go xs = case xs of
      x:y:z:rest -> go2 x y z
      _ -> error "This is ambiguous"
    go2 x y z
      | x==y = z-x
      | x==z = y-x
      | otherwise = x-y

findBadElement towerMap root = go childStats
  where
    childStats = zip children childWeights
    Just (_,children) = M.lookup root towerMap
    childWeights = getChildWeights towerMap root
    go ((kx,wx):(ky,wy):(kz,wz):rest)
      | (wx==wy) && (wx/=wz) = Just (kz,wz)
      | (wx==wz) && (wx/=wy) = Just (ky,wy)
      | (wy==wz) && (wy/=wx) = Just (kx,wx)
      | otherwise = go2 (kx,wx) rest
    go _ = Nothing
    go2 x [] = Nothing
    go2 (k,w) ((k',w'):xs)
      | w==w' = go2 (k,w) xs
      | otherwise = Just (k',w')

findWantedValue towerMap diff root =
  case (findBadElement towerMap root) of
    Nothing -> w-diff
    Just (k,w) -> findWantedValue towerMap diff k
  where
    Just (w,_) = M.lookup root towerMap

main = do
  content <- (readFile . head) =<< getArgs
  let towers = parseTowers content
  let root = findRoot towers
  putStrLn $ "root: " ++ root
  let towerMap = M.fromList towers
  let difference = findDifference towerMap root
  putStrLn $ "difference: " ++ show difference
  putStrLn $ "wanted value: " ++ show (findWantedValue towerMap difference root)
