#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec])" -i runhaskell
module Main where

import Data.List (sort)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . result . (runRounds 20) . parseInput

-- Parser
parseInput :: String -> [Monkey]
parseInput s =
  case parse monkeysP "input" s of
    Right ms -> ms
    Left e -> error . show $ e

type Item = Int
data Expr = EInt Int | Old deriving (Show, Eq, Ord)
data Op = Add Expr | Mul Expr deriving (Show, Eq, Ord)
type MonkeyNo = Int
type Test = (Int, MonkeyNo, MonkeyNo)
data Monkey = Monkey { mNo :: MonkeyNo, mItems :: [Item], mOp :: Op, mTest :: Test, mInspCount :: Int }
  deriving (Show, Eq, Ord)

monkeysP :: Parser [Monkey]
monkeysP = monkeyP `sepBy` newline

monkeyP :: Parser Monkey
monkeyP = do
  nmo <- monkeyNoP
  startItems <- startItemsP
  operation <- operationP
  tst <- testP
  pure $ Monkey nmo startItems operation tst 0

num :: Parser Int
num = read <$> many1 digit

monkeyNoP :: Parser MonkeyNo
monkeyNoP = string "Monkey " *> num <* string ":" <* newline

startItemsP :: Parser [Item]
startItemsP = string "  Starting items: " *> (num `sepBy1` string ", ") <* newline

operationP :: Parser Op
operationP = string "  Operation: new = old " *> (mulP <|> addP) <* newline
  where
    exprP = (pure Old <* string "old" <|> EInt <$> num)
    mulP = Mul <$> (string "* " *> exprP)
    addP = Add <$> (string "+ " *> exprP)

testP :: Parser Test
testP = do
  string "  Test: divisible by "
  d <- num
  newline
  string "    If true: throw to monkey "
  tm <- num
  newline
  string "    If false: throw to monkey "
  fm <- num
  newline
  pure $ (d, tm, fm)

-- Runner
type Throw = (MonkeyNo, Item)

runMonkey :: Monkey -> (Monkey, [Throw])
runMonkey m@(Monkey _ [] _ _ _) = (m, [])
runMonkey m = throw . test . releave . inspect $ m
  where
    inspect = runOp (mOp m) . head . mItems
    runOp (Add Old) w = 2*w
    runOp (Add (EInt b)) w = w+b
    runOp (Mul Old) w = w*w
    runOp (Mul (EInt b)) w = w*b
    releave = (`div` 3)
    (d, tm, fm) = mTest m
    test w = (if w `mod` d == 0 then tm else fm, w)
    newMonkey = m { mItems = tail (mItems m), mInspCount = mInspCount m + 1 }
    throw t = let (nm, ts) = runMonkey newMonkey in (nm, t:ts)

runOneMonkey :: Int -> [Monkey] -> [Monkey]
runOneMonkey n ms = modify n (const nm) nms
  where
    (nm, throws) = runMonkey $ ms !! n
    nms = foldr land ms (reverse throws)
    land (no, w) ms = modify no (\m -> m { mItems = mItems m ++ [w] }) ms

runRound :: [Monkey] -> [Monkey]
runRound ms = applyAll runs $ ms
  where
    runs = [ runOneMonkey n | n <- [0..length ms-1] ]
    applyAll [] = id
    applyAll (f:fs) = applyAll fs . f

runRounds :: Int -> [Monkey] -> [Monkey]
runRounds 0 ms = ms
runRounds n ms = runRounds (n-1) (runRound ms)

modify :: Int -> (a -> a) -> [a] -> [a]
modify 0 f (a:as) = f a:as
modify n f (a:as) = a:modify (n-1) f as

result :: [Monkey] -> Int
result = product . take 2 . reverse . sort . map mInspCount

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then " Ok" else "\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg res = putStrLn $ msg ++ ":" ++ res

  let monkey0 = Monkey 0 [79, 98] (Mul $ EInt 19) (23, 2, 3) 0
  let testMonkeys = parseInput testinput
  t "parse 0" $ head testMonkeys `eq` monkey0
  t "runMonkey 0" $ runMonkey monkey0 `eq` (monkey0 { mItems = [], mInspCount = 2 }, [(3,500),(3,620)])
  let afterOneRun = runOneMonkey 0 testMonkeys
  t "runOneMonkey 0" $ head (afterOneRun) `eq` monkey0 { mItems = [], mInspCount = 2 }
  t "runOneMonkey 3" $ (afterOneRun !! 3) `eq` Monkey 3 [74, 500, 620] (Add $ EInt 3) (17, 0, 1) 0
  t "runRound" $ (map mItems) (runRound testMonkeys) `eq` [[20, 23, 27, 26],[2080, 25, 167, 207, 401, 1046],[],[]]
  t "process" $ process testinput `eq` "10605"

  putStrLn "-- End of tests --"
