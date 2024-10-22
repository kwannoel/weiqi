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
removePieces coords board = foldl' (\board' coord -> removeSinglePiece coord board') board coords

removeSinglePiece :: (X, Y) -> Board -> Board
removeSinglePiece coord board = M.update (\_ -> Just Empty) coord board

-- Board dynamics
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

-- Piece Location
adjacentCoords :: (X, Y) -> [(X, Y)]
adjacentCoords (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

expand :: [(X, Y)] -> [(X, Y)]
expand coords = concatMap adjacentCoords coords

-- Piece meta data
opposite :: Piece -> Maybe Piece
opposite Black = Just White
opposite White = Just Black
opposite Empty = Nothing

checkQi :: [(X, Y)] -> Piece -> Board -> Qi
checkQi coord piece board = go [] (Right coord) piece board
  where go checked (Right []) _ _ = Dead checked
        go _ (Left _) _ _ = Alive
        go checked (Right coords) piece board =
          go (checked ++ coords)
             sameOrEmpty
             piece
             board
          where unCheckedCoords = filter (isNotChecked checked) (expand coords)
                emptyOrCoords = (hasEmpty piece board unCheckedCoords)
                sameOrEmpty = case emptyOrCoords of
                  Left Empty -> Left Empty 
                  Right coords' -> Right $ filter (isSame piece board) coords'

isSame :: Piece -> Board -> (X, Y) -> Bool
isSame piece board coord =
  case M.lookup coord board of
    Just val -> val == piece
    _ -> False

isNotChecked :: [(X, Y)] -> (X, Y) -> Bool
isNotChecked checked = \x -> not $ elem x checked

hasEmpty :: Piece -> Board -> [(X, Y)] -> Either Piece [(X, Y)]
hasEmpty piece board coords | length (filter (isEmpty board) coords) == 0 = Right coords
                            | otherwise = Left Empty
  where isEmpty :: Board -> (X, Y) -> Bool
        isEmpty board coord =
          case M.lookup coord board of
            Just Empty -> True
            _ -> False
