module Weiqi.Dynamics where

import Data.Foldable
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Weiqi.Types
import Weiqi.Dynamics.Internal
import Weiqi.Dynamics.Validate

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
  | checkValid piece coord board = Right $ insertPieceAtCoord coord piece board
  | otherwise = Left $ "Invalid placement at: " ++ show coord ++ " " ++ show piece

-- Remove pieces
removePiece :: (X, Y) -> BoardState ()
removePiece = undefined
