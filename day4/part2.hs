#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.split])" -i runhaskell
-- vim: sw=2 expandtab
module Main where

import Data.List.Split (splitOneOf)

main :: IO ()
main = putStrLn . process . lines =<< readFile "input"

process :: [String] -> String
process = show . length . filter (==True) .  map (overlapsEither . parse)

type Range = (Int, Int)

parse :: String -> (Range, Range)
parse s = ((a,b),(c,d))
  where
    [a,b,c,d] = map read $ splitOneOf ",-" s

overlaps :: (Range, Range) -> Bool
overlaps ((x,y),(a,b)) = x <= a && a <= y

overlapsEither :: (Range, Range) -> Bool
overlapsEither (a,b) = overlaps (a,b) || overlaps (b,a)

