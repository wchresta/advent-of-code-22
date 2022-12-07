#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.parsec p.containers])" -i runhaskell
-- vim: sw=2 expandtab
module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

main :: IO ()
main = do
  tests
  putStrLn . process . parse fileOpP "file" =<< readFile "input"

data FileOp = Cd String | Ls | DirList String | FileList String Int deriving (Show, Eq, Ord)

process :: Either ParseError [FileOp] -> String
process (Right (_:ops)) = let ([], t) = runFileOps ops (Dir "/" []) in processTree t
process (Left err) = show err

processTree :: FileTree -> String
processTree = show . sumSmall . sizes

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


data FileTree = File String Int | Dir String [FileTree] deriving (Show, Eq, Ord)

runFileOps :: [FileOp] -> FileTree -> ([FileOp], FileTree)
runFileOps [] t = ([], t)
runFileOps (Ls:ops) t = runFileOps ops t
runFileOps (Cd "..":ops) t = (ops, t)
runFileOps (Cd dir:ops) (Dir root index) = runFileOps leftOps (Dir root (newDir:index))
  where
    (leftOps, newDir) = runFileOps ops (Dir dir [])
runFileOps (DirList _:ops) t = runFileOps ops t
runFileOps (FileList name size:ops) (Dir dir index) = runFileOps ops (Dir dir (File name size:index))

sizes :: FileTree -> [Int]
sizes = map snd . sizes'
  where
    sizes' (File _ size) = [(False, size)]
    sizes' (Dir _ index) = (True, totSize):dirs
      where
        ss = concatMap sizes' index
        dirs = filter (\(t,_) -> t) $ ss
        totSize = sum . map snd $ ss

sumSmall :: [Int] -> Int
sumSmall = sum . filter (<100000)

tests :: IO ()
tests = do
  let eq got want = putStrLn $ if got == want then "Ok" else "Got " ++ show got ++ ", want " ++ show want

  parse fileOpP "" "$ ls\n" `eq` Right [Ls]
  parse fileOpP "" "$ cd a\n" `eq` Right [Cd "a"]
  parse fileOpP "" "$ cd /\n" `eq` Right [Cd "/"]
  parse fileOpP "" "dir a\n" `eq` Right [DirList "a"]
  parse fileOpP "" "12345 a.txt\n" `eq` Right [FileList "a.txt" 12345]

  runFileOps [FileList "a" 1, Cd "b", FileList "c" 2] (Dir "/" [])
    `eq` ([], Dir "/" [Dir "b" [File "c" 2], File "a" 1])
  runFileOps [Cd "a", Cd "b", FileList "c" 1] (Dir "/" [])
    `eq` ([], Dir "/" [Dir "a" [Dir "b" [File "c" 1]]])

  putStrLn "-- End of tests --"
