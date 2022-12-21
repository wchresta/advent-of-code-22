#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec p.unordered-containers])" -i runhaskell
module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . calc "root" . parseInput

type MName = String
type Monkeys = M.HashMap MName MExpr
data MExpr = MInt Int | MAdd MName MName | MSub MName MName | MMul MName MName | MDiv MName MName
  deriving (Show, Eq, Ord)

parseInput :: String -> Monkeys
parseInput s = case parse monkeysP "" s of
  Left err -> error . show $ err
  Right ms -> ms

monkeysP :: Parser Monkeys
monkeysP = M.fromList <$> monkeyP `sepEndBy1` newline
  where
    monkeyP = (,) <$> wordP <* string ": " <*> mExprP
    mExprP = (MInt <$> numP) <|> mOpP
    mOpP = do
      lm <- wordP
      space
      op <- oneOf "+-/*"
      space
      rm <- wordP
      let constr = case op of
                    '+' -> MAdd
                    '-' -> MSub
                    '*' -> MMul
                    '/' -> MDiv
      pure $ constr lm rm
    numP = read <$> many1 digit
    wordP = many1 letter

calc :: MName -> Monkeys -> Int
calc n m = case m M.! n of
  MInt x -> x
  MAdd x y -> calc x m + calc y m
  MSub x y -> calc x m - calc y m
  MMul x y -> calc x m * calc y m
  MDiv x y -> calc x m `div` calc y m

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "solve" $ process testinput `eq` "152"

  putStrLn "-- End of tests --"
