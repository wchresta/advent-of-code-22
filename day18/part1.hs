#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.split p.unordered-containers])" -i runhaskell
module Main where

import Data.List.Split
import qualified Data.HashSet as S

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . countSides . mkPts . parseInput

type Pt = (Int, Int, Int)

parseInput :: String -> [Pt]
parseInput = map ((\[x,y,z] -> (read x, read y, read z)) . splitOn ",") . lines

mkPts :: [Pt] -> S.HashSet Pt
mkPts = S.fromList

countBordering :: S.HashSet Pt -> Int
countBordering pts = count (1,0,0) + count (0,1,0) + count (0,0,1)
  where
    count (i,j,k) = S.size $ S.intersection (S.map (\(x,y,z) -> (x+i,y+j,z+k)) pts) pts

countSides :: S.HashSet Pt -> Int
countSides hs = 6 * S.size hs - 2 * countBordering hs

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "solve" $ process testinput `eq` "64"

  putStrLn "-- End of tests --"
