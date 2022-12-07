#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec p.containers])" -i runhaskell
-- vim: sw=2 expandtab
module Main where

import Debug.Trace
import Data.List (minimumBy)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

main :: IO ()
main = do
  tests
  content <- readFile "input"
  output <- process content
  putStrLn output

data FileOp = Cd String | Ls | DirList String | FileList String Int deriving (Show, Eq, Ord)

process :: String -> IO String
process s = do
  let tree = processOps . parseOps $ s
  pure . processTree $ tree

parseOps :: String -> [FileOp]
parseOps s = case parse fileOpP "infile" s of
  Right ops -> ops
  Left err -> error (show err)

processOps :: [FileOp] -> FileTree
processOps ops = let ([], t) = runFileOps ops EmptyTree in t

processTree :: FileTree -> String
processTree t = let (Dir _ _ size) = findToDelete t in show size

fileOpP :: Parser [FileOp]
fileOpP = many1 $ choice [commandP, dirListP, fileListP] <* endOfLine

commandP :: Parser FileOp
commandP = string "$ " >> (lsP <|> cdP)

lsP :: Parser FileOp
lsP = string "ls" *> (pure Ls)

cdP :: Parser FileOp
cdP = string "cd " *> (Cd <$> fileNameP)

fileNameP :: Parser String
fileNameP = choice [string "..", string "/", many1 (letter <|> char '.')]

dirListP :: Parser FileOp
dirListP = string "dir " *> (DirList <$> fileNameP)

fileListP :: Parser FileOp
fileListP = do
  size <- read <$> many1 digit
  space
  fileName <- fileNameP
  pure $ FileList fileName size

data FileTree = EmptyTree | File String Int | Dir String [FileTree] Int deriving (Eq, Ord)

instance Show FileTree where
  show = unlines . showLine
    where
      showLine :: FileTree -> [String]
      showLine EmptyTree = ["EmptyTree"]
      showLine (File name size) = ["file " ++ name ++ " " ++ show size]
      showLine (Dir name index size) = ("dir " ++ name ++ " " ++ show size) : map (\l -> "  " ++ l) (concatMap showLine index)

runFileOps :: [FileOp] -> FileTree -> ([FileOp], FileTree)
runFileOps [] t = ([], t)
runFileOps (Ls:ops) t = runFileOps ops t
runFileOps (Cd "..":ops) t = (ops, t)
runFileOps (Cd "/":ops) EmptyTree = runFileOps ops (Dir "/" [] 0)
runFileOps (Cd dir:ops) (Dir root index s) = runFileOps leftOps $ Dir root (innerDir:index) (s+innerSize)
  where
    (leftOps, innerDir@(Dir _ _ innerSize)) = runFileOps ops (Dir dir [] 0)
runFileOps (DirList _:ops) t = runFileOps ops t
runFileOps (FileList name size:ops) (Dir dir index s) = runFileOps ops (Dir dir (File name size:index) (s+size))
runFileOps ops t = error $ show ops ++ "\n" ++ show t

tot :: Int
tot = 70000000

need :: Int
need = 30000000

findToDelete :: FileTree -> FileTree
findToDelete root@(Dir "/" dir curr) = smallest
  where
    haveFree = tot - curr
    toFree = need - haveFree

    possible = findMatching root
    smallest = minimumBy (\(Dir _ _ a) (Dir _ _ b) -> compare a b) possible

    findMatching :: FileTree -> [FileTree]
    findMatching d@(Dir _ idx size) = if size > toFree then d:concatMap findMatching idx else []
    findMatching _ = []
findToDelete r = error $ "Non exhaust " ++ show r

tests :: IO ()
tests = do
  let eq got want = putStrLn $ if got == want then "Ok" else "Got " ++ show got ++ ", want " ++ show want

  parse fileOpP "" "$ ls\n" `eq` Right [Ls]
  parse fileOpP "" "$ cd a\n" `eq` Right [Cd "a"]
  parse fileOpP "" "$ cd /\n" `eq` Right [Cd "/"]
  parse fileOpP "" "dir a\n" `eq` Right [DirList "a"]
  parse fileOpP "" "12345 a.txt\n" `eq` Right [FileList "a.txt" 12345]

  putStrLn "- runFileOps"

  runFileOps [FileList "x" 1] (Dir "a" [] 0) `eq` ([], Dir "a" [File "x" 1] 1)
  runFileOps [Cd "b"] (Dir "a" [] 0) `eq` ([], Dir "a" [Dir "b" [] 0] 0)

  runFileOps [Cd "/", FileList "a" 1, Cd "b", FileList "c" 2] EmptyTree
    `eq` ([], Dir "/" [Dir "b" [File "c" 2] 2, File "a" 1] 3)
  runFileOps [Cd "/", Cd "a", Cd "b" , FileList "c" 1] EmptyTree
    `eq` ([], Dir "/" [Dir "a" [Dir "b" [File "c" 1] 1] 1] 1)

  putStrLn "-- End of tests --"
