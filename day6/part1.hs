#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.split p.containers])" -i runhaskell
{-# language TupleSections #-}
-- vim: sw=2 expandtab
module Main where

import qualified Data.Map.Strict as M
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  tests
  putStrLn . process . lines =<< readFile "input"

process :: [String] -> String
process = show . head . map findStartPos

cChunk :: Int -> [a] -> [[a]]
cChunk n as | length as < n = []
cChunk n as = take n as:cChunk n (tail as)

countUnique :: Ord a => [a] -> Int
countUnique = M.size . M.fromListWith (+) . map (,1)

findStartPos :: String -> Int
findStartPos = (+4) . fromJust . elemIndex 4 . map countUnique . cChunk 4

tests :: IO ()
tests = do
  let eq got want = putStrLn $ if got == want then "Ok" else "Got " ++ show got ++ ", want " ++ show want

  countUnique [1,1,3,4] `eq` 3
  cChunk 3 [1..4] `eq` [[1,2,3],[2,3,4]]
  process ["bvwbjplbgvbhsrlpgdmjqwftvncz"] `eq` "5"

  putStrLn "-- End of tests --"
