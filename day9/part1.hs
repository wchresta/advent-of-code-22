#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.Vec])" -i runhaskell
module Main where

import qualified Data.Vec as V
import Data.Vec ((:.)((:.)))
import Data.List (nub)

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

type Pos = V.Vec2 Int
data S = S { headPos :: Pos, tailPos :: Pos, tailHist :: [Pos] }
  deriving (Show, Eq, Ord)

process :: String -> String
process s = show . length . nub . totalTailHist . foldl runMove start . map parse . lines $ s

totalTailHist :: S -> [Pos]
totalTailHist s = tailPos s:tailHist s

data Dir = L | R | U | D deriving (Show, Read, Eq, Ord)
type Move = (Dir, Int)

parse :: String -> Move
parse (m:' ':cnt) = (read . pure $ m, read cnt)

zero :: Pos
zero = 0 :. 0

start :: S
start = S zero zero []

dir :: Dir -> V.Vec2 Int
dir U = 0 :. 1
dir R = 1 :. 0
dir D = 0 :. (-1)
dir L = (-1) :. 0

runMove :: S -> Move -> S
runMove s (d, n) = catchUp $ s { headPos = headPos s + (V.map (*n) $ dir d) }
runMove s _ = s

touch :: S -> Bool
touch s = (-1) <= x diff && x diff <= 1 && (-1) <= y diff && y diff <= 1
  where
    diff = headPos s - tailPos s

x :: Pos -> Int
x = V.getElem 0

y :: Pos -> Int
y = V.getElem 1

catchUp :: S -> S
catchUp s | touch s = s
catchUp (S h t hist) = catchUp $ S h newTail (t:hist)
  where
    dx = case compare (x t) (x h) of EQ -> 0; LT -> 1; GT -> (-1)
    dy = case compare (y t) (y h) of EQ -> 0; LT -> 1; GT -> (-1)
    newTail = t + (dx:.dy)

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = putStrLn $ if got == want then "Ok" else "Got " ++ show got ++ ", want " ++ show want
  process testinput `eq` "13"
  putStrLn "-- End of tests --"
