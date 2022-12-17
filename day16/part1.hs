#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec])" -i runhaskell
module Main where

import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . solve . parseInput

parseInput :: String -> Valves
parseInput s = case parse linesP "" s of
    Right res -> res
    Left err -> error . show $ err
  where
    linesP = toValves <$> (valveP `sepEndBy1` newline) <* eof
    toValves vs = M.fromList [ (vName v, v) | v <- vs ]
    valveP = Valve <$> (string "Valve " *> nameP)
                   <*> (string " has flow rate=" *> numP)
                   <*> (string "; " *> plural "tunnel" *> string " "
                                    *> plural "lead" *> string " to "
                                    *> plural "valve" *> string " "
                                    *> (nameP `sepBy1` string ", "))
    plural :: String -> Parser ()
    plural t = string t *> optional (char 's') *> pure ()
    numP = read <$> many1 digit
    nameP = many1 letter

type VName = String
data Valve = Valve { vName :: VName, vRate :: Int, vEdges :: [String] }
  deriving (Show, Eq, Ord)
type Valves = M.Map VName Valve

workingV :: Valves -> [String]
workingV = M.keys . M.filter ((>0) . vRate)

findDists :: Valves -> VName -> M.Map VName Int
findDists vs n = f [(0,vs M.! n)] $ M.singleton n 0
  where
    f :: [(Int, Valve)] -> M.Map VName Int -> M.Map VName Int
    f [] dists = dists
    f ((d,r):drs) dists = f (drs ++ newDrs) $ dists `M.union` newDists
      where
        es = filter (`M.notMember` dists) . vEdges $ r
        newDists = M.fromList [(n,d+1) | n <- es]
        newDrs = map (\n -> (d+1,vs M.! n)) $ es

type MajorMap = M.Map VName (M.Map VName Int)
findMajor :: Valves -> MajorMap
findMajor vs = f working M.empty
  where
    f [] dists = dists
    f (n:ns) dists = f ns $ M.insert n newDists dists
      where newDists = M.filterWithKey (\k _ -> isWorking k && k /= n) . findDists vs $ n
    working = workingV vs
    workingS = S.fromList working
    isWorking n = n `S.member` workingS

type TimeLeft = Int
type CurrPos = VName
type Score = Int

walk :: Valves -> MajorMap -> S.Set VName -> CurrPos -> TimeLeft -> Score
walk _ _ _ _ t | t < 0 = error $ "Out of time" ++ show t
walk _ _ _ _ 0 = 0
walk vs mm opened curr time
  | curr `S.notMember` opened = score + walk vs mm (S.insert curr opened) curr (time-1)
  | otherwise                 = case map f . filter (\(v,d) -> d+1 <= time && v `S.notMember` opened) . M.toList $ mm M.! curr of
    [] -> 0
    vs -> maximum vs
  where
    score = vRate (vs M.! curr) * (time-1)
    f :: (VName, Int) -> Score
    f (n,d) = walk vs mm opened n (time-d)

solve :: Valves -> Score
solve vs = maximum . map f $ majors
  where
    aaDists = findDists vs "AA"
    mm = findMajor vs
    majors = M.keys mm
    f n = walk vs mm S.empty n (30-aaDists M.! n)

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "parse" $ parseInput "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    `eq` M.fromList [("AA", Valve "AA" 0 ["DD", "II", "BB"])]
  t "working" $ (workingV . parseInput) testinput
    `eq` ["BB", "CC", "DD", "EE", "HH", "JJ"]
  t "findDists" $ (findDists (parseInput testinput) "AA" M.! "JJ") `eq` 2
  t "solve" $ process testinput `eq` "1651"

  putStrLn "-- End of tests --"
