#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.split p.array])" -i runhaskell
-- vim: sw=2 expandtab
module Main where

import Data.Tuple (swap)
import Data.Array as A
import Data.List (isPrefixOf, transpose)
import Data.List.Split (chunksOf, splitOn, splitOneOf)

main :: IO ()
main = do
  tests
  putStrLn . process . lines =<< readFile "input"

process :: [String] -> String
process = answer . execute . parse

type Stack = [Char]
type Stacks = [Stack]
type Move = (Int, Int, Int)
type Prob = (Stacks, [Move])

parse :: [String] -> Prob
parse lines = (toStack stackLines, reverse moves)
  where
    (stackLines, moves) = foldl pLines ([],[]) lines
    pLines :: Prob -> String -> Prob
    pLines (s,m) line | (isPrefixOf "[" . dropWhile (==' ')) line = (pStackLine line:s,m)
    pLines (s,m) line | isPrefixOf "move" line = (s,pMove line:m)
    pLines sm _ = sm

    pStackLine = map (head . drop 1) . chunksOf 4
    pMove move = let [_, mno, _, from, _, to ] = words move in (read mno, read from, read to)

    toStack = map (dropWhile (==' ') . reverse) . transpose

execute :: Prob -> [Stack]
execute (s,ms) = foldl move s ms

-- 
--   v     v
-- 1 2 3 4 5 6
move :: Stacks -> Move -> Stacks
move ss (n,f,t) = left ++ newLower : mid ++ newUpper : right
  where
    l = min f t
    u = max f t
    (left, lower:midRight) = splitAt (l-1) ss
    (mid, upper:right) = splitAt (u-l-1) midRight

    maySwap = if f < t then id else swap
    (from, to) = maySwap (lower, upper)
    (toMove, newFrom) = splitAt n from
    newTo = reverse toMove ++ to
    (newLower, newUpper) = maySwap (newFrom, newTo)

answer :: Stacks -> String
answer = map head

tests :: IO ()
tests = do
  testData <- lines <$> readFile "testfile"
  let eq got want = putStrLn $ if got == want then "Ok" else "Got " ++ show got ++ ", want " ++ show want

  parse testData `eq` (["NZ","DCM","P"],[(1,2,1),(3,1,3),(2,2,1),(1,1,2)])
  move ["","","","A",""] (1,4,1) `eq` ["A","","","",""]
  move ["A","","","",""] (1,1,4) `eq` ["","","","A",""]
  move ["NZ","DCM","P"] (1,2,1) `eq` ["DNZ","CM","P"]
  move ["ABC","DEF"] (2,1,2) `eq` ["C","BADEF"]
  move ["DNZ","CM","P"] (3,1,3) `eq` ["","CM","ZNDP"]
  execute (parse testData) `eq` ["C","M","ZNDP"]

  putStrLn "-- End of tests --"
