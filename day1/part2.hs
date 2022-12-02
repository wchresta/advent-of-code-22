-- vim: sw=2 expandtab
module Main where

import System.IO (getContents)
import Data.List (foldl', sort)

main :: IO ()
main = putStr . unlines . process . lines =<< getContents

process :: [String] -> [String]
process = pure . show . sum . topN 3 . sumByElves

sumByElves :: [String] -> [Integer]
sumByElves = foldl' collectElves [0]
  where
    collectElves es "" = 0:es
    collectElves (e:es) x = (read x+e):es
    
topN :: Int -> [Integer] -> [Integer]
topN n = foldl' calcTop (repeat 0)
  where
    calcTop top = take n . insertTop top

    insertTop (a:as) x | x > a = x:a:as
    insertTop (a:as) x = a:insertTop as x
    insertTop as _ = as
