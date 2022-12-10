#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.deepseq])" -i runhaskell
module Main where

import Control.DeepSeq
import Debug.Trace
import Data.List (nub, foldl')
import qualified Data.Set as S

main :: IO ()
main = do
  tests
  putStrLn . result . process =<< readFile "input"

type Pos = (Int, Int)
type Hist = S.Set Pos
data S = S { rope :: [Pos], tailHist :: Hist }
  deriving (Show, Eq, Ord)

process :: String -> S
process s = foldl' runMove (start 10) . map parse . lines $ s

result :: S -> String
result = show . S.size . tailHist

data Dir = L | R | U | D deriving (Show, Read, Eq, Ord)
type Move = (Dir, Int)

parse :: String -> Move
parse (m:' ':cnt) = (read . pure $ m, read cnt)

zero :: Pos
zero = (0,0)

start :: Int -> S
start n = S (take n $ repeat zero) S.empty

dir :: Dir -> (Int, Int)
dir U = (0,1)
dir R = (1,0)
dir D = (0,(-1))
dir L = ((-1),0)

runMove :: S -> Move -> S
runMove (S ((x,y):ts) hist) (d, n) = catchUp $ deepseq hist $ S newRope hist
  where
    (dx,dy) = dir d
    newRope = (x + n * dx, y + n * dy):ts

touch :: Pos -> Pos -> Bool
touch (hx,hy) (tx,ty) = (-1) <= dx && dx <= 1 && (-1) <= dy && dy <= 1
  where
    (dx, dy) = (hx-tx,hy-ty)

x :: Pos -> Int
x = fst

y :: Pos -> Int
y = snd

catchUp :: S -> S
catchUp (S r h) = let (nr, nh, m) = catchUpOnce r in (if m then catchUp else id) $ deepseq (nh,h) $ S nr (nh `S.union` h)

catchUpOnce :: [Pos] -> ([Pos], Hist, Bool)
catchUpOnce [] = ([], S.empty, False)
catchUpOnce [x] = ([x], S.singleton x, False)
catchUpOnce (h:t@(tx,ty):rs) = (newRope, newHist, hasMoved)
  where
    dx = case compare (x t) (x h) of EQ -> 0; LT -> 1; GT -> (-1)
    dy = case compare (y t) (y h) of EQ -> 0; LT -> 1; GT -> (-1)
    newDir = (dx,dy)
    thisHasMoved = newDir /= zero && (not $ touch h t)
    newTail = (tx+dx,ty+dy)
    (newInnerRope, newInnerHist, innerHasMoved) = catchUpOnce (if thisHasMoved then newTail:rs else t:rs)
    newRope = h:newInnerRope
    newHist = if rs == [] && thisHasMoved then S.insert t newInnerHist else newInnerHist
    hasMoved = thisHasMoved || innerHasMoved

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  testinput2 <- readFile "testinput2"
  let eq got want = putStrLn $ if got == want then "Ok" else "Got  " ++ show got ++ "\nWant " ++ show want

  catchUp (S [(2,2),zero,zero,zero] S.empty) `eq` S [(2,2),(1,1),zero,zero] (S.fromList [zero])
  catchUp (S [(3,3),zero,zero,zero] S.empty) `eq` S [(3,3),(2,2),(1,1),zero] (S.fromList [zero])
  catchUp (S [(4,3),zero,zero,zero] S.empty) `eq` S [(4,3),(3,3),(2,2),(1,1)] (S.fromList [(1,1),zero])
  catchUp (S [(5,3),zero,zero,zero] S.empty) `eq` S [(5,3),(4,3),(3,3),(2,2)] (S.fromList [(2,2),(1,1),zero])
  catchUp (S [(5,5),zero,zero,zero] S.empty) `eq` S [(5,5),(4,4),(3,3),(2,2)] (S.fromList [(2,2),(1,1),zero])
  catchUp (S [(10,0),zero,zero,zero] S.empty) `eq`
    S [(10,0),(9,0),(8,0),(7,0)] (S.fromList [ (x,0) | x <- [7,6..0] ])

  (head . rope . process $ testinput2) `eq` ((-11),15)
  (tailHist . process $ "R 5\nU 8") `eq` (S.fromList [zero])
  (result . process) testinput `eq` "1"
  (result . process) testinput2 `eq` "36"

  putStrLn "-- End of tests --"
