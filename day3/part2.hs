-- vim: sw=2 expandtab
module Main where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import qualified Data.Set as S

main :: IO ()
main = do
  tests
  putStrLn . process . lines =<< readFile "input"

process :: [String] -> String
process = show . sum . map (priority . findCommon) . chunksOf 3

type Item = Char
type Rucksack = String


priority :: Item -> Int
priority x =
  case ord x - 64 of
    d | d <= 26 -> d + 26
    d -> d - 32

findCommon :: [Rucksack] -> Item
findCommon rs = S.elemAt 0 $ a `S.intersection` b `S.intersection` c
  where
    [a,b,c] = map (S.fromList) rs

tests :: IO ()
tests = do
  let eq a b = putStrLn $ if a == b then "Ok" else "Got " ++ show a ++ ", want " ++ show b
  priority 'a' `eq` 1
  priority 'z' `eq` 26
  priority 'A' `eq` 27
  priority 'Z' `eq` 52
