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
process = show . solve . parseInput

type MName = String
type Monkeys = M.HashMap MName Expr
data Op = OAdd | OSub | OMul | ODiv | OEql
  deriving (Show, Eq, Ord)
data Expr = EInt Int
          | EOp Op MName MName
          | EVar
          | EDep Int Expr  -- Used to solve
  deriving (Show, Eq, Ord)

parseInput :: String -> Monkeys
parseInput s = case parse monkeysP "" s of
  Left err -> error . show $ err
  Right ms -> ms

monkeysP :: Parser Monkeys
monkeysP = M.fromList <$> monkeyP `sepEndBy1` newline
  where
    monkeyP = do
      name <- wordP
      string ": "
      expr <- mExprP
      let expr' = case name of
                   "root"    -> let EOp _ a b = expr in EOp OEql a b
                   "humn"    -> EVar
                   otherwise -> expr
      pure (name, expr')
    mExprP = (EInt <$> numP) <|> mOpP
    mOpP = do
      lm <- wordP
      space
      opChar <- oneOf "+-/*"
      space
      rm <- wordP
      let op = case opChar of
                    '+' -> OAdd
                    '-' -> OSub
                    '*' -> OMul
                    '/' -> ODiv
      pure $ EOp op lm rm
    numP = read <$> many1 digit
    wordP = many1 letter

solve :: Monkeys -> Int
solve m = case calc x m of
            Just xVal -> findVar xVal y m
            Nothing -> let Just yVal = calc y m in findVar yVal x m
  where
    EOp OEql x y = m M.! "root"

calc :: MName -> Monkeys -> Maybe Int
calc n m = case m M.! n of
  EInt x -> Just x
  EOp OAdd x y -> (+) <$> calc x m <*> calc y m
  EOp OSub x y -> (-) <$> calc x m <*> calc y m
  EOp OMul x y -> (*) <$> calc x m <*> calc y m
  EOp ODiv x y -> div <$> calc x m <*> calc y m
  EVar -> Nothing

findVar :: Int -> MName -> Monkeys -> Int
findVar t n m = case m M.! n of
  EVar       -> t
  EInt n     -> n
  EOp op x y ->
    case calc x m of
      Just xVal -> findVar (calcNewTarget t op (Left xVal)) y m
      Nothing -> let Just yVal = calc y m in findVar (calcNewTarget t op (Right yVal)) x m
    where
      -- t = x + y
      calcNewTarget t OAdd (Left x) = t - x
      calcNewTarget t OAdd (Right y) = t - y
      -- t = x - y
      calcNewTarget t OSub (Left x) = x - t
      calcNewTarget t OSub (Right y) = t + y
      -- t = x * y
      calcNewTarget t OMul (Left x) = t `div` x
      calcNewTarget t OMul (Right y) = t `div` y
      -- t = x / y
      calcNewTarget t ODiv (Left x) = x `div` t
      calcNewTarget t ODiv (Right y) = t * y


tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "solve" $ process testinput `eq` "301"

  putStrLn "-- End of tests --"
