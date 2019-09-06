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
insertPieceAtCoord :: (X, Y) -> PieceInfo -> Board -> Board
insertPieceAtCoord coord piece board = M.insert coord piece board

-- Piece insertion steps:
-- 1. Validate
-- 2. Insert
-- 3. Update Board

-- Validating before proceeding with insertion of a piece
-- Update board if required as well

insertPiece :: (X, Y) -> Piece -> Board -> Either Error Board
insertPiece coord piece board
  | checkValid piece coord board = Right $ insertAndUpdate coord piece board
  | otherwise = Left $ "Invalid placement at: " ++ show coord ++ " " ++ show (colour piece)

-- Unsafe inserting and updating of connect piece data
insertAndUpdate :: (X, Y) -> Piece -> Board -> Board
insertAndUpdate coord piece board = let adjacentPieces' = adjacentPieces coord piece board
                                        uniquePieces = uniqueCluster adjacentPieces' board
                                        updatedCluster = updateCluster uniquePieces coord board
                                        updatedPiece = Piece (colour piece) (updatedCluster)
                                        updatedBoard = updateBoard updatedCluster piece board
                                    in  updatedBoard

updateBoard :: [(X, Y)] -> Piece -> Board -> Board
updateBoard cluster piece board = foldl'
                                  (\b c -> insertPieceAtCoord c (PieceInfo piece) b)
                                  board
                                  cluster

updateCluster :: [(X, Y)] -> (X, Y) -> Board -> [(X, Y)]
updateCluster coords coord board = coord : concatMap (\c -> getCluster c board) coords

getCluster :: (X, Y) -> Board -> [(X, Y)]
getCluster coord board = let Just (PieceInfo piece) = M.lookup coord board
                         in  connected piece

-- Gets unique piece clusters of the same color
uniqueCluster :: [(X, Y)] -> Board -> [(X, Y)]
uniqueCluster [] _ = []
uniqueCluster coords board = let coordsWithValues :: [((X, Y), PieceInfo)]
                                 coordsWithValues =
                                   map (\coord -> keyValPair coord board) coords
                                 uniqueValues = foldl' addUnique [] coordsWithValues
                             in  map fst uniqueValues

addUnique :: [((X, Y), PieceInfo)] -> ((X, Y), PieceInfo) -> [((X, Y), PieceInfo)]
addUnique accum current | elem (snd current) (map snd accum) = accum
                        | otherwise = current : accum

-- Returns key: value pair of a coordinate-key
keyValPair :: (X, Y) -> Board -> ((X, Y), PieceInfo)
keyValPair coord board = let Just val = M.lookup coord board
                         in  (coord, val)

-- Gets adjacent non-empty pieces of the same color
adjacentPieces :: (X, Y) -> Piece -> Board -> [(X, Y)]
adjacentPieces (x, y) piece board = filter (\coord -> sameColor coord piece board)
                                           (adjacentCoords (x, y))

adjacentCoords :: (X, Y) -> [(X, Y)]
adjacentCoords (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x + 1, y)]

sameColor :: (X, Y) -> Piece -> Board -> Bool
sameColor coord piece board =
  case M.lookup coord board of
    Nothing -> False
    Just pieceInfo -> filterDifferent pieceInfo (colour piece) 

filterDifferent :: PieceInfo -> Colour -> Bool
filterDifferent pieceInfo colour' =
  case pieceInfo of
    Empty -> False
    PieceInfo piece -> colour piece == colour'

checkValid :: Piece -> (X, Y) -> Board -> Valid
checkValid piece coord board =
  case M.lookup coord board of
    Just Empty -> True
    _ -> False

-- Remove pieces
removePiece :: (X, Y) -> BoardState ()
removePiece (x, y) = do
  board <- get
  put (removeCluster (x, y) board)

removePieces :: [(X, Y)] -> Board -> Board
removePieces coords board = foldl' (\board coord -> removeSinglePiece coord board) board coords

removeCluster :: (X, Y) -> Board -> Board
removeCluster coord board = let Just (PieceInfo piece) = M.lookup coord board
                                connectedPieces = connected piece 
                            in  removePieces connectedPieces board

removeSinglePiece :: (X, Y) -> Board -> Board
removeSinglePiece coord board = M.update (\_ -> Just Empty) coord board
