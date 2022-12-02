-- vim: sw=2 expandtab
module Main where

import System.IO (getContents)
import Data.List (foldl')

data Move = R | P | S deriving (Show, Eq)

type Round = (Move, Move)

main :: IO ()
main = putStr . unlines . process . lines =<< getContents

process :: [String] -> [String]
process = pure . show . sum . map (score . parse)

parse :: String -> Round
parse (a:_:[b]) = (parseMove a, parseMove b)
parse err = error $ "Cannot parse " ++ err

parseMove :: Char -> Move
parseMove 'A' = R
parseMove 'X' = R
parseMove 'B' = P
parseMove 'Y' = P
parseMove 'C' = S
parseMove 'Z' = S

choiceScore :: Round -> Int
choiceScore (_, R) = 1
choiceScore (_, P) = 2
choiceScore (_, S) = 3

winScore :: Round -> Int
winScore (R, P) = 6
winScore (P, S) = 6
winScore (S, R) = 6
winScore (x, y) | x == y = 3
winScore _ = 0

score :: Round -> Int
score r = winScore r + choiceScore r

