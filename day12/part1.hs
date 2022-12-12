#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.array])" -i runhaskell
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Array
import Data.Char (ord)
import Data.List (sort)
import qualified Data.Set as S

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . findShortest . parse

type Coord = (Int, Int)
type B = Array Coord Int

parse :: String -> B
parse s = listArray ((0,0),(height-1,width-1)) . concatMap (map conv) $ ls
  where
    ls = lines s
    height = length ls
    width = length . head $ ls

    conv :: Char -> Int
    conv 'S' = 0
    conv 'E' = 27
    conv x = ord x - ord 'a' + 1

findStart :: B -> Coord
findStart = fst . head . filter (\(i,e) -> e == 0) . assocs

type Dist = Int

dfs :: B -> [(Coord, Dist)] -> S.Set Coord -> Int
dfs b [] visited = error "Ended search without finding exit"
dfs b ((i,d):is) visited
  | b ! i == 27 = d
  | i `S.member` visited = dfs b is visited
  | otherwise = dfs b nis (S.insert i visited)
      where
        newDir = findSteps b i
        nis = is ++ (map (,d) . filter (`S.notMember` visited) $ newDir)

findShortest :: B -> Int
findShortest b = dfs b [(findStart b, 0)] S.empty

findSteps :: B -> Coord -> [Coord]
findSteps b (i,j) = reachable . valid $ [(i+1,j),(i-1,j),(i,j-1),(i,j+1)]
  where
    bds = bounds b
    c = b ! (i,j)
    valid = filter (\kl -> inRange bds kl)
    reachable = filter (\kl -> b ! kl <= c+1)

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "findTest" $ (findStart . parse $ testinput) `eq` (0,0)
  t "process" $ process testinput `eq` "31"

  putStrLn "-- End of tests --"
