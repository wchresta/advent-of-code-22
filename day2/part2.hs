-- vim: sw=2 expandtab
module Main where

import System.IO (getContents)
import Data.List (foldl')

data Move = R | P | S deriving (Show, Eq)

data Result = W | D | L deriving (Show, Eq)

type Pattern = (Move, Result)
type Round = (Move, Move)

main :: IO ()
main = putStr . unlines . process . lines =<< getContents

process :: [String] -> [String]
process = pure . show . sum . map (score . evalPattern . parse)

parse :: String -> Pattern
parse (a:_:[b]) = (parseMove a, parseResult b)
parse err = error $ "Cannot parse " ++ err

parseMove :: Char -> Move
parseMove 'A' = R
parseMove 'B' = P
parseMove 'C' = S

parseResult :: Char -> Result
parseResult 'X' = L
parseResult 'Y' = D
parseResult 'Z' = W

evalPattern :: Pattern -> Round
evalPattern p@(m, _) = (m, chooseMove p)

chooseMove :: Pattern -> Move
chooseMove (R, W) = P
chooseMove (R, L) = S
chooseMove (P, W) = S
chooseMove (P, L) = R
chooseMove (S, W) = R
chooseMove (S, L) = P
chooseMove (x, D) = x

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

