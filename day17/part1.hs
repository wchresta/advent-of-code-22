#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.Vec])" -i runhaskell
module Main where

import Data.Vec ((:.)(..))
import qualified Data.Vec as V
import qualified Data.Set as S

main :: IO ()
main = do
  tests
  putStrLn . process =<< readFile "input"

process :: String -> String
process = show . solve . parseInput

data Board = Board { bHeight :: Int, bTiles :: (S.Set Pos) }
  deriving (Eq, Ord, Show)
type SwayBand = [Char]
type PieceBand = [PieceType]
parseInput :: String -> SwayBand
parseInput = head . lines

type Pos = V.Vec2 Int
data PieceType = Wide | Plus | L | I | Box
  deriving (Eq, Ord, Show)
data Piece = Piece { pType :: PieceType, pPos :: Pos }
  deriving (Eq, Ord, Show)

typePos :: PieceType -> [Pos]
typePos Wide = [0:.0,0:.1,0:.2,0:.3]
typePos Plus = [0:.1,1:.0,1:.1,1:.2,2:.1]
typePos L = [0:.0,0:.1,0:.2,1:.2,2:.2]
typePos I = [0:.0,1:.0,2:.0,3:.0]
typePos Box = [0:.0,0:.1,1:.0,1:.1]

typeWidth :: PieceType -> Int
typeWidth Wide = 4
typeWidth Plus = 3
typeWidth L = 3
typeWidth I = 1
typeWidth Box = 2

typeHeight :: PieceType -> Int
typeHeight Wide = 1
typeHeight Plus = 3
typeHeight L = 3
typeHeight I = 4
typeHeight Box = 2

piecePos :: Piece -> S.Set Pos
piecePos (Piece t p) = S.map (p+) . S.fromList . typePos $ t

pieceTop :: Piece -> Int
pieceTop (Piece t (h:._)) = typeHeight t + h

collides :: Board -> Piece -> Bool
collides _ (Piece t (i:.j:.()))
  | i < 0 || j < 0 || (j+typeWidth t-1) >= 7 = True  -- Collide with wall
collides b p = not $ bTiles b `S.disjoint` piecePos p

printPiece :: Piece -> Board -> Board
printPiece p b = Board { bHeight = max (bHeight b) (pieceTop p)
                       , bTiles = bTiles b `S.union` piecePos p
                       }

moveDown :: Piece -> Piece
moveDown (Piece t pos) = Piece t $ pos + ((-1):.0)

sway :: Char -> Piece -> Piece
sway '>' (Piece t (i:.j)) = Piece t (i:.(j+1))
sway '<' (Piece t (i:.j)) = Piece t (i:.(j-1))

move :: Piece -> SwayBand -> Board -> (SwayBand, Board)
move p (s:ss) b =
  let
    swayedPiece = sway s p
    actualSwayedPiece = if collides b swayedPiece then p else swayedPiece
    movedPiece = moveDown actualSwayedPiece
   in if collides b movedPiece
    then (ss, printPiece actualSwayedPiece b)
    else move movedPiece ss b

dropNPieces :: Int -> [Char] -> Board
dropNPieces cnt sways =
    f (cycle sways) (cycle [Wide, Plus, L, I, Box]) (Board 0 S.empty) cnt
  where
    f _ _ b 0 = b
    f ss (p:ps) b n =
        let (ss', b') = move (Piece p ((bHeight b+3):.2)) ss b
         in f ss' ps b' (n-1)

solve :: [Char] -> Int
solve = bHeight . dropNPieces 2022

printBoard :: Board -> String
printBoard b = unlines . map printLine $ [bHeight b,bHeight b-1..0]
  where
    printLine i = '|':[ if (i:.j:.()) `S.member` bTiles b then '#' else ' ' | j <- [0..6] ]++"|"

tests :: IO ()
tests = do
  testinput <- readFile "testinput"
  let eq got want = if got == want then ": Ok" else ": FAILED\n Got  " ++ show got ++ "\n Want " ++ show want
  let t msg = putStrLn . (msg ++)

  t "solve" $ process (parseInput testinput) `eq` "3068"

  putStrLn "-- End of tests --"
