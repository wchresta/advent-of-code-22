#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.Unique])" -i runhaskell
module Main where

import Data.Char (digitToInt)
import Data.List (transpose, sort)
import Data.List.Unique (sortUniq)


main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . length . sortUniq . findAllVisible . addIndex 1 . parse

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

type Cell = (Int, Int)
addIndex :: Int -> [[Int]] -> [[Cell]]
addIndex n [] = []
addIndex n (a:as) = zip [n..] a : addIndex (n + length a) as

height :: Cell -> Int
height = snd

findAllVisible :: [[Cell]] -> [Cell]
findAllVisible cs = concatMap findGridVisible
  [ cs
  , map reverse cs
  , transpose cs
  , map reverse . transpose $ cs
  ]

findGridVisible :: [[Cell]] -> [Cell]
findGridVisible = concatMap findLineVisible

findLineVisible :: [Cell] -> [Cell]
findLineVisible = findLineVisible' (-1)
  where
    findLineVisible' m [] = []
    findLineVisible' m (a:as) | height a > m = a:findLineVisible' (height a) as
    findLineVisible' m (_:as) = findLineVisible' m as

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = putStrLn $ if got == want then "Ok" else "Got " ++ show got ++ ", want " ++ show want
  addIndex 1 [[1,2],[2,3]] `eq` [[(1,1),(2,2)],[(3,2),(4,3)]]
  findLineVisible [(1,1),(2,2),(3,3)] `eq` [(1,1),(2,2),(3,3)]
  findLineVisible [(1,1),(2,2),(3,2)] `eq` [(1,1),(2,2)]
  findLineVisible [(1,3),(2,2),(3,2)] `eq` [(1,3)]
  findLineVisible [(1,1),(2,3),(3,3)] `eq` [(1,1),(2,3)]
  (findGridVisible . addIndex 1 . parse) "30373" `eq` [(1,3),(4,7)]
  (findGridVisible . addIndex 1 . parse) "37303" `eq` [(1,3),(2,7)]
  process testinput `eq` "21"
  putStrLn "-- End of tests --"
