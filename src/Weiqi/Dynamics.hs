module Weiqi.Dynamics where

import Data.Foldable
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Weiqi.Types

placePiece :: (X, Y) -> Piece -> BoardState ()
placePiece coord piece = do
  board <- get
  case insertPiece coord piece board of
    Left err -> liftIO $ putStrLn err
    Right updatedBoard -> put updatedBoard

-- Unsafe inserting of pieces
insertPieceAtCoord :: (X, Y) -> Piece -> Board -> Board
insertPieceAtCoord coord piece board = M.insert coord piece board

-- Piece insertion steps:
-- 1. Validate
-- 2. Insert
-- 3. Update Board

-- Validating before proceeding with insertion of a piece
-- Update board if required as well

insertPiece :: (X, Y) -> Piece -> Board -> Either Error Board
insertPiece coord piece board
  | checkValid piece coord board = Right $ insertPieceAtCoord coord piece board
  | otherwise = Left $ "Invalid placement at: " ++ show coord ++ " " ++ show piece

updateBoard :: [(X, Y)] -> Piece -> Board -> Board
updateBoard cluster piece board = foldl'
                                  (\b c -> insertPieceAtCoord c piece b)
                                  board
                                  cluster

addUnique :: [((X, Y), Piece)] -> ((X, Y), Piece) -> [((X, Y), Piece)]
addUnique accum current | elem (snd current) (map snd accum) = accum
                        | otherwise = current : accum

-- Returns key: value pair of a coordinate-key
keyValPair :: (X, Y) -> Board -> ((X, Y), Piece)
keyValPair coord board = let Just val = M.lookup coord board
                         in  (coord, val)

adjacentCoords :: (X, Y) -> [(X, Y)]
adjacentCoords (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x + 1, y)]

sameColor :: (X, Y) -> Piece -> Board -> Bool
sameColor coord piece board =
  case M.lookup coord board of
    Nothing -> False
    Just piece' -> piece == piece' 

checkValid :: Piece -> (X, Y) -> Board -> Valid
checkValid piece coord board =
  case M.lookup coord board of
    Just Empty -> True
    _ -> False

-- Remove pieces
removePiece :: (X, Y) -> BoardState ()
removePiece = undefined

removePieces :: [(X, Y)] -> Board -> Board
removePieces coords board = foldl' (\board coord -> removeSinglePiece coord board) board coords

removeSinglePiece :: (X, Y) -> Board -> Board
removeSinglePiece coord board = M.update (\_ -> Just Empty) coord board
