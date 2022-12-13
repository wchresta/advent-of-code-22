#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec])" -i runhaskell
{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (sort)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

divider1 :: M
divider1 = MS [MS [MI 2]]

divider2 :: M
divider2 = MS [MS [MI 6]]

process :: String -> String
process = show
        . product
        . map fst
        . filter (\(i,d) -> d == divider1 || d == divider2)
        . zip [1..]
        . sort
        . (divider1:)
        . (divider2:)
        . parseInput

data M = MS [M] | MI Int deriving (Show, Eq)

parseInput :: String -> [M]
parseInput s = case parse inputP "" s of
  Left err -> error . show $ err
  Right ms -> ms

inputP :: Parser [M]
inputP = (messageP `sepEndBy1` many endOfLine) <* eof

messageP :: Parser M
messageP =
  (MI . read <$> many1 digit) <|>
  (MS <$> between (string "[") (string "]") (messageP `sepBy` string ","))

instance Ord M where
  compare (MI a) (MI b) = compare a b
  compare (MS []) (MS (_:_)) = LT
  compare (MS []) (MS []) = EQ
  compare (MS (_:_)) (MS []) = GT
  compare (MS (a:as)) (MS (b:bs)) =
    case compare a b of
      EQ -> compare (MS as) (MS bs)
      x  -> x
  compare a@(MI _) b@(MS _) = compare (MS [a]) b
  compare a@(MS _) b@(MI _) = compare a (MS [b])

findLTs :: [(M,M)] -> [Int]
findLTs = map fst . filter (\(i,(a,b)) -> compare a b == LT) . zip [1..]

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "parse1" $ parseInput "[1,1,3,1,1]\n[1,1,5,1,1]" `eq`
      [MS [MI 1,MI 1,MI 3,MI 1,MI 1],MS [MI 1,MI 1,MI 5,MI 1,MI 1]]
  t "parse2" $ parseInput "[1]\n[1]" `eq`
      [MS [MI 1], MS [MI 1]]
  t "parse3" $ parseInput "[1]\n[1]\n\n[1]\n[1]" `eq`
      [MS [MI 1], MS [MI 1], MS [MI 1], MS [MI 1]]
  t "process" $ process testinput `eq` "140"

  putStrLn "-- End of tests --"
