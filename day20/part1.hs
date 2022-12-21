#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.array])" -i runhaskell
module Main where

import Data.Ix (range)
import Debug.Trace
import Data.Array

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . result . mix . parseInput

parseInput :: String -> [Int]
parseInput = map read . lines

mix :: [Int] -> ([Int], [Int])
mix as = (\(a,b) -> (elems a, elems b)) . applyAll moves $ (mas, listArray bnds $ range bnds)
  where
    n = length as
    applyAll fs a = foldl (\a f -> f a) a fs
    bnds = (0,n-1)
    mas = listArray bnds as
    f x = (x+n) `mod` n
    idxMove i j = case compare i j of
      EQ -> id
      LT -> \k -> if k == i then j else if i < k && k <= j then (k-1) else k
      GT -> \k -> if k == i then j else if j <= k && k < i then (k+1) else k
    ridxMove i j = case compare i j of
      EQ -> id
      LT -> \k -> if k == j then i else if i <= k && k < j then (k+1) else k
      GT -> \k -> if k == j then i else if j < k && k <= i then (k-1) else k
    move i (as,is) = (newAs, newIs)
      where
        newAs = ixmap bnds (ridxMove i2 j2) as
        newIs = fmap (idxMove i2 j2) is
        i2 = is ! i
        el = as ! i2
        j2 = if i2+el<=0 then f (i2+el-1) else if i2+el >= n then f (i2+el+1) else f (i2+el)
    moves = map move . range $ bnds

result :: ([Int], [Int]) -> Int
result (xs,_) = sum [ xs !! ((zeroIdx + i) `mod` length xs) | i <- [1000,2000,3000] ]
  where
    zeroIdx = fst . head . filter ((0==) . snd) . zip [0..] $ xs

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg res = do putStr $ msg ++ "\n"; putStrLn res

  t "process" $ process testinput `eq` "3"

  putStrLn "-- End of tests --"
