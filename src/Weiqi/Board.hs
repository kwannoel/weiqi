module Weiqi.Board where

import Data.Foldable
import qualified Data.Map.Strict as M

import Weiqi.Dynamics
import Weiqi.Types

-- An empty row of length x
emptyRow :: Int -> [Int] -> Board -> Board
emptyRow y xs boardSeed =
  foldl' (\boardAccum x -> insertPieceAtCoord (x, y) Empty boardAccum)
         boardSeed
         xs

-- An emptyBoard of dimension (x, y)
emptyBoard :: [Int] -> [Int] -> Board
emptyBoard ys xs =
  foldl' (\boardAccum y -> emptyRow y xs boardAccum)
         M.empty
         ys

board9x9 :: Board
board9x9 = emptyBoard [0..8] [0..8]
