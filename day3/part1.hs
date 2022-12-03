-- vim: sw=2 expandtab
module Main where

import Data.Char (ord)
import qualified Data.Set as S

main :: IO ()
main = do
  tests
  putStrLn . process . lines =<< readFile "input"

process :: [String] -> String
process = show . sum . map (priority . findDouble . parse)

type Item = Char
type Compartment = String
type Rucksack = (String, String)

parse :: String -> Rucksack
parse s = splitAt (length s `div` 2) s

priority :: Item -> Int
priority x =
  case ord x - 64 of
    d | d <= 26 -> d + 26
    d -> d - 32

findDouble :: Rucksack -> Item
findDouble (l,r) = S.elemAt 0 same
  where
    (ls, rs) = (S.fromList l, S.fromList r)
    same = S.intersection ls rs

tests :: IO ()
tests = do
  let eq a b = putStrLn $ if a == b then "Ok" else "Got " ++ show a ++ ", want " ++ show b
  parse "vJrwpWtwJgWrhcsFMMfFFhFp" `eq` ("vJrwpWtwJgWr", "hcsFMMfFFhFp")
  findDouble ("vJrwpWtwJgWr", "hcsFMMfFFhFp") `eq` 'p'
  priority 'a' `eq` 1
  priority 'z' `eq` 26
  priority 'A' `eq` 27
  priority 'Z' `eq` 52
