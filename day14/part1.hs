#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec p.array p.split])" -i runhaskell
{-# LANGUAGE TupleSections #-}
module Main where

import Data.List.Split (chunksOf)
import Data.List (transpose)
import Data.Array
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show
        . snd
        . dropAllSand source
        . makeBoard
        . parseInput

type Pt = (Int, Int)

parseInput :: String -> [[Pt]]
parseInput s = case parse inputP "" s of
  Left err -> error . show $ err
  Right ms -> ms

inputP :: Parser [[Pt]]
inputP = (lineP `sepEndBy1` endOfLine) <* eof
  where
    lineP = ptP `sepBy1` string " -> "
    ptP = (\x y -> (y,x)) <$> numP <* string "," <*> numP
    numP = read <$> many1 digit

data C = A | R | S deriving (Eq, Ord)
instance Show C where
  show A = "."
  show R = "#"
  show S = "o"

type Board = Array Pt C

showBoard :: Board -> String
showBoard b
  = unlines
  . chunksOf width
  . map (head . show)
  . elems $ b
    where
      width = (\((i,j),(k,l)) -> l-j+1) . bounds $ b

source :: Pt
source = (0,500)

findBounds :: [Pt] -> (Pt, Pt)
findBounds = foldr f (source,source)
  where
    f (i,j) ((ai,aj),(bi,bj)) = ((min ai i, min aj j), (max bi i, max bj j))

makeBoard :: [[Pt]] -> Board
makeBoard ls = drawLines (makeEmptyBoard ls) ls

makeEmptyBoard :: [[Pt]] -> Board
makeEmptyBoard ls = emptyBoard
  where
    bounds = findBounds . concat $ ls
    emptyBoard = listArray bounds [A | _ <- range bounds]

drawLines :: Board -> [[Pt]] -> Board
drawLines = foldr drawLine
  where
    drawLine [_] b = b
    drawLine [] b = b
    drawLine ((i,j):(k,l):is) b =
      drawLine ((k,l):is) $ b//[ ((x,y),R) | x <- rng i k, y <- rng j l ]
    rng i j = [min i j..max i j]

dropSand :: Board -> Pt -> (Board, Bool)
dropSand b ij@(i,j)
    | not (inRange bnd ij) = (b, False)
    | b ! ij /= A = (b, False)
    | otherwise = tryGo [(i+1,j),(i+1,j-1),(i+1,j+1)]
  where
    bnd = bounds b
    tryGo [] = (b//[(ij,S)], True)
    tryGo (kl:kls)
      | inRange bnd kl && b ! kl == A = dropSand b kl
      | not (inRange bnd kl)          = (b, False)
      | otherwise                     = tryGo kls

dropAllSand :: Pt -> Board -> (Board, Int)
dropAllSand ij b =
  case dropSand b ij of
    (b', True) -> let (b'', n) = dropAllSand ij b' in (b'',n+1)
    (b', False) -> (b',0)

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "parse1" $ parseInput "0,0 -> 1,1" `eq` [[(0,0),(1,1)]]
  t "lines" $ (findBounds . concat . parseInput $ testinput) `eq` ((0,494),(9,503))
  t "emptyBoard" $ (length . elems . makeEmptyBoard . parseInput $ testinput) `eq` 100
  t "process" $ (process testinput) `eq` "24"

  putStrLn "-- End of tests --"
