module Weiqi.Dynamics where

import Data.Foldable
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Weiqi.Types
import Weiqi.Dynamics.Internal
import Weiqi.Validate

-- Piece insertion steps:
-- 1. Validate
-- 2. Insert
-- 3. Update Board

-- Validating before proceeding with insertion of a piece
-- Update board if required as well

placePiece :: (X, Y) -> Piece -> BoardState ()
placePiece coord piece = do
  board <- get
  case insertPiece coord piece board of
    Left err -> liftIO $ putStrLn err
    Right updatedBoard -> put updatedBoard

insertPiece :: (X, Y) -> Piece -> Board -> Either Error Board
insertPiece coord piece board
  | checkValid piece coord board = Right $ checkAndUpdateSurrounding coord piece intermediateBoard
  | otherwise = Left $ "Invalid placement at: " ++ show coord ++ " " ++ show piece
  where intermediateBoard = insertPieceAtCoord coord piece board
  
checkAndUpdateSurrounding :: (X, Y) -> Piece -> Board -> Board
checkAndUpdateSurrounding coord piece board =
  foldl'
  (\board coord -> handlePiece board coord piece)
  board
  (adjacentCoords coord)

-- Only if opposite colour do qi check
handlePiece :: Board -> (X, Y) -> Piece -> Board
handlePiece board coord piece =
  case M.lookup coord board of
    Just piece' -> if Just piece' == opposite piece
                   then checkAndUpdate coord piece' board
                   else board
    _ -> board

checkAndUpdate :: (X, Y) -> Piece -> Board -> Board
checkAndUpdate coord piece board =
  case checkQi [coord] piece board of
    Dead coords -> removePieces coords board
    Alive -> board
