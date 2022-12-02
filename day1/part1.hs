-- vim: sw=2 expandtab
module Main where

import System.IO (getContents)
import Data.List (foldl')

main :: IO ()
main = putStr . unlines . process . lines =<< getContents

process :: [String] -> [String]
process = pure . show . maximum . sumByElves

sumByElves :: [String] -> [Integer]
sumByElves = foldl' collectElves [0]
  where
    collectElves es "" = 0:es
    collectElves (e:es) x = (read x+e):es
    
