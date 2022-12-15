#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec p.criterion])" -i runhaskell
{-# LANGUAGE TupleSections #-}
module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Criterion
import Criterion.Main (defaultMain)

main :: IO ()
main = do
  tests
  putStrLn . process 2000000 (-400000, 5500000) =<< readFile "input"

process :: Int -> (Int, Int) -> String -> String
process y rng
  = show
  . countEmpty (mkLine y rng)
  . parseInput

data Pos = P Int Int deriving (Show, Ord, Eq)
data Sen = Sen Pos Pos Int deriving (Show, Ord, Eq)

parseInput :: String -> [Sen]
parseInput s = case parse linesP "" s of
    Right res -> res
    Left err -> error . show $ err
  where
    linesP = (lineP `sepEndBy1` newline) <* eof
    mkSen s b = Sen s b (dist s b)
    lineP = mkSen <$> (string "Sensor at " *> posP)
                <*> (string ": closest beacon is at " *> posP)
    posP = P <$> (string "x=" *> numP)
             <*> (string ", y=" *> numP)
    numP = read <$> many1 (char '-' <|> digit)

dist :: Pos -> Pos -> Int
dist (P x1 y1) (P x2 y2) = abs (x2-x1) + abs (y2-y1)

isBetween :: Sen -> Pos -> Bool
isBetween (Sen s _ r) p = dist p s <= r

isBetweenAny :: [Sen] -> Pos -> Bool
isBetweenAny ss p = any (flip isBetween p) ss

collides :: Pos -> [Sen] -> Bool
collides p = any (\(Sen s b _) -> b == p)

countEmpty :: [Pos] -> [Sen] -> Int
countEmpty ps ss = (length $ findEmpty ps ss) - countBeacons ps ss

findEmpty :: [Pos] -> [Sen] -> [Pos]
findEmpty ps ss = filter (isBetweenAny ss) $ ps

mkLine :: Int -> (Int, Int) -> [Pos]
mkLine y (lo, hi) = [P x y | x <- [lo..hi]]

countBeacons :: [Pos] -> [Sen] -> Int
countBeacons ps ss = length . filter (flip collides ss) $ ps

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "parse1" $ parseInput "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" `eq` [Sen (P 2 18) (P (-2) 15) 7]
  t "dist" $ dist (P 8 7) (P 2 10) `eq` 9
  t "dist" $ dist (P 8 7) (P (-1) 7) `eq` 9
  t "dist" $ dist (P 8 7) (P (-2) 7) `eq` 10
  t "isBetween" $ isBetween (Sen (P 8 7) (P 2 10) 9) (P 9 8) `eq` True
  t "isBetween" $ isBetween (Sen (P 8 7) (P 2 10) 9) (P (-1) 7) `eq` True
  t "isBetween" $ isBetween (Sen (P 8 7) (P 2 10) 9) (P (-2) 7) `eq` False
  t "isBetween" $ isBetween (Sen (P 8 7) (P 2 10) 9) (P 12 1) `eq` False
  t "isBetween" $ isBetween (Sen (P 8 7) (P 2 10) 9) (P 12 2) `eq` True
  t "isBetween" $ isBetween (Sen (P 8 7) (P 2 10) 9) (P 11 1) `eq` True
  t "process" $ (countEmpty (mkLine 10 (-5,30)) . parseInput $ testinput) `eq` 26

{-
  input <- readFile "input"
  defaultMain
    [ bench "200" $ whnf (process 2000000 (-4000, 200)) input
    , bench "2000" $ whnf (process 2000000 (-4000, 2000)) input
    , bench "20000" $ whnf (process 2000000 (-4000, 20000)) input
    ]
-}
  putStrLn "-- End of tests --"
