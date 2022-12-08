#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.Unique])" -i runhaskell
module Main where

import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . maximum . lookEverywhere . parse

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

lookEverywhere :: [[Int]] -> [Int]
lookEverywhere as = map (\(i,j) -> lookAround i j as) $ [ (i,j) | i <- [0..h-1], j <- [0..w-1] ]
  where
    h = length as
    w = length . head $ as

lookAround :: Int -> Int -> [[Int]] -> Int
lookAround i j as = product . map (length . look s) . around i j $ as
  where
    s = (as !! i) !! j
    w = length . head $ as

around :: Int -> Int -> [[Int]] -> [[Int]]
around i j as = [up, right, down, left]
  where
    right = drop (j+1) (as !! i)
    left = reverse $ take j (as !! i)
    down = drop (i+1) (transpose as !! j)
    up = reverse $ take i (transpose as !! j)

look :: Int -> [Int] -> [Int]
look h [] = []
look h (a:as) | a >= h = [a]
look h (a:as) = a : look h as

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = putStrLn $ if got == want then "Ok" else "Got " ++ show got ++ ", want " ++ show want

  let cells = parse testinput
  around 0 0 cells `eq` [[], [0,3,7,3], [2,6,3,3], []]
  around 1 1 cells `eq` [[0], [5, 1, 2], [5, 3, 5], [2]]
  around 1 2 cells `eq` [[3], [1, 2], [3, 5, 3], [5, 2]]
  map (length . look 5) [[3], [1, 2], [3, 5, 3], [5, 2]] `eq` [1, 2, 2, 1]
  look 5 [3, 5, 3] `eq` [3, 5]
  lookAround 1 2 cells `eq` 4
  lookAround 3 2 cells `eq` 8
  process testinput `eq` "8"
  putStrLn "-- End of tests --"
