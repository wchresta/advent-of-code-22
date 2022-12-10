#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [])" -i runhaskell
module Main where

import Debug.Trace
import Data.List (foldl')

main :: IO ()
main = do
  tests
  putStrLn . result . process =<< readFile "input"

data Instr = Noop | AddX Int deriving (Show, Eq, Ord)
type S = CPU

process :: String -> S
process = run start . map parse . lines

parse :: String -> Instr
parse "noop" = Noop
parse ('a':'d':'d':'x':' ':num) = AddX . read $ num

type X = Int
type Tick = Int
type CPU = [(X, Tick)]

start :: CPU
start = [(1,0)]

run :: CPU -> [Instr] -> CPU
run c = foldl' runInstr c

runInstr :: CPU -> Instr -> CPU
runInstr hs@((x,t):_) Noop = (x,t+1):hs
runInstr hs@((x,t):_) (AddX n) = (x+n,t+2):hs

result :: CPU -> String
result = show . signalStrength

signalStrength :: CPU -> Int
signalStrength [] = 0
signalStrength [(x,t)] = 0
signalStrength ((x,t):(y,s):rs) = signal + signalStrength ((y,s):rs)
  where
    onCycle n = (n-20) `mod` 40 == 0

    signal =
      if onCycle t
        then y*t
        else if onCycle (t-1) && not (onCycle s)
          then y*(t-1)
          else 0

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  testinput2 <- readFile "testinput2"
  let eq got want = putStrLn $ if got == want then "Ok" else "Got  " ++ show got ++ "\nWant " ++ show want

  (map parse . lines) testinput `eq` [Noop, AddX 3, AddX (-5)]
  parse "addx -11" `eq` AddX (-11)
  runInstr start Noop `eq` [(1,1),(1,0)]
  runInstr start (AddX 3) `eq` [(4,2),(1,0)]

  (result . process) testinput2 `eq` "13140"

  putStrLn "-- End of tests --"
