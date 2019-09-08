module Weiqi.Dynamics.Internal where

import Data.Foldable
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Weiqi.Types

-- Unsafe inserting of pieces
insertPieceAtCoord :: (X, Y) -> Piece -> Board -> Board
insertPieceAtCoord coord piece board = M.insert coord piece board

sameColor :: (X, Y) -> Piece -> Board -> Bool
sameColor coord piece board =
  case M.lookup coord board of
    Nothing -> False
    Just piece' -> piece == piece' 

-- Remove pieces
removePieces :: [(X, Y)] -> Board -> Board
removePieces coords board = foldl' (\board coord -> removeSinglePiece coord board) board coords

removeSinglePiece :: (X, Y) -> Board -> Board
removeSinglePiece coord board = M.update (\_ -> Just Empty) coord board

-- ??? Misc
updateBoard :: [(X, Y)] -> Piece -> Board -> Board
updateBoard cluster piece board = foldl'
                                  (\b c -> insertPieceAtCoord c piece b)
                                  board
                                  cluster

addUnique :: [((X, Y), Piece)] -> ((X, Y), Piece) -> [((X, Y), Piece)]
addUnique accum current | elem (snd current) (map snd accum) = accum
                        | otherwise = current : accum

keyValPair :: (X, Y) -> Board -> ((X, Y), Piece)
keyValPair coord board = let Just val = M.lookup coord board
                         in  (coord, val)

adjacentCoords :: (X, Y) -> [(X, Y)]
adjacentCoords (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x + 1, y)]

