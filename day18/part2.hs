#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.split p.containers])" -i runhaskell
module Main where

import Debug.Trace
import Data.List.Split
import Data.Ix (index, inRange, range)
import Data.Foldable (toList)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Graph as G

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . solve . parseInput

type Pt = (Int, Int, Int)

parseInput :: String -> [Pt]
parseInput = map ((\[x,y,z] -> (read x, read y, read z)) . splitOn ",") . lines

type Dim = (Int, Int, Int)
solve :: [Pt] -> Int
solve pts = traceShow (bounds, outerLava, outerNeighbours) $ allLavaEdges
  where
    findDim f = foldr1 (\(x,y,z) (mx,my,mz) -> (f mx x, f my y, f mz z)) pts
    minDim@(minx, miny, minz) = findDim min
    maxDim@(maxx, maxy, maxz) = findDim max
    bounds = (minDim, maxDim)
    idx = toVertex bounds
    pSet = S.fromList pts
    edges (dx,dy,dz) = concatMap (\(from,to) -> [(from,to),(to,from)])
                     . map (\(from,to) -> (idx from, idx to))
                     . filter (\(from,to) -> to `S.notMember` pSet && from `S.notMember` pSet)
                     . filter (\(_,to) -> inRange bounds to)
                     . map (\(x,y,z) -> ((x,y,z),(x+dx,y+dy,z+dz)))
                     $ range bounds
    allEdges = concatMap edges [(1,0,0), (0,1,0), (0,0,1)]

    outerShell = concat
      [ [(x,y,z) | x <- [minx..maxx], y <- [minx..maxy], z <- [minz,maxz] ]
      , [(x,y,z) | x <- [minx..maxx], z <- [minz..maxz], y <- [miny,maxy] ]
      , [(x,y,z) | y <- [miny..maxy], z <- [minz..maxz], x <- [minx,maxx] ]
      ]
    outerExposed = map idx . filter (`S.notMember` pSet) $ outerShell
    outerLava = filter (`S.member` pSet) outerShell
    outerNeighbours = filter (not . inRange bounds) . concatMap neighbours $ outerLava
    outerLavaCount = length . filter (not . inRange bounds) . concatMap neighbours $ outerLava

    gBounds = (idx minDim, idx maxDim)
    g = G.buildG gBounds allEdges
    exposed = S.fromList . concatMap toList . G.dfs g $ outerExposed

    neighbours (x,y,z) = concat [ [ (x+dx,y,z) | dx <- [-1,1] ]
                                , [ (x,y+dy,z) | dy <- [-1,1] ]
                                , [ (x,y,z+dz) | dz <- [-1,1] ]
                                ]

    lavaBorderingCount = M.fromListWith (+)
                       . map (\e -> (idx e, 1))
                       . filter (inRange bounds)
                       . concatMap neighbours
                       $ pts
    lavaEdges = sum . map (\e -> M.findWithDefault 0 e lavaBorderingCount) $ S.toList exposed
    allLavaEdges = lavaEdges + outerLavaCount

type Bounds = (Pt, Pt)
toVertex :: Bounds -> Pt -> G.Vertex
toVertex = index

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "solve" $ process testinput `eq` "58"

  putStrLn "-- End of tests --"
