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
  | checkValid piece coord board = Right $ insertAndUpdate coord piece board
  | otherwise = Left $ "Invalid placement at: " ++ show coord ++ " " ++ show piece

insertAndUpdate :: (X, Y) -> Piece -> Board -> Board
insertAndUpdate coord piece board = updatedBoard
  where intermediateBoard = insertPieceAtCoord coord piece board
        updatedBoard = case checkFour [coord] piece board of
          Dead coords -> removePieces coords intermediateBoard
          Alive -> intermediateBoard

checkFour :: [(X, Y)] -> Piece -> Board -> Qi
checkFour coord piece board = go [] (Right coord) piece board
  where go checked (Right []) _ _ = Dead checked
        go _ (Left _) _ _ = Alive
        go checked (Right coords) piece board =
          go (checked ++ coords)
             sameOrEmpty
             piece
             board
          where unCheckedCoords = filter (isChecked checked) (expand coords)
                emptyOrCoords = (hasEmpty piece board unCheckedCoords)
                sameOrEmpty = case emptyOrCoords of
                  Left Empty -> Left Empty 
                  Right ls   -> Right $ filter (isSame piece board) ls

isSame :: Piece -> Board -> (X, Y) -> Bool
isSame piece board coord =
  case M.lookup coord board of
    Just val -> val == piece
    _ -> False

isChecked :: [(X, Y)] -> (X, Y) -> Bool
isChecked checked = \x -> not $ elem x checked

hasEmpty :: Piece -> Board -> [(X, Y)] -> Either Piece [(X, Y)]
hasEmpty piece board coords | length (filter (isEmpty board) coords) == 0 = Right coords
                            | otherwise = Left Empty
  where isEmpty :: Board -> (X, Y) -> Bool
        isEmpty board coord =
          case M.lookup coord board of
            Just Empty -> True
            _ -> False

expand :: [(X, Y)] -> [(X, Y)]
expand coords = concatMap adjacentCoords coords

-- Remove pieces
removePiece :: (X, Y) -> BoardState ()
removePiece = undefined
