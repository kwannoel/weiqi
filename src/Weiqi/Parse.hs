module Weiqi.Parse where

import qualified Text.Read as R
import Data.Maybe

import Weiqi.Validate
import Weiqi.Types

parse :: String -> Maybe (X, Y, Piece)
parse input | verify input = Just . convert . words $ input
            | otherwise = Nothing

convert :: [String] -> (X, Y, Piece)
convert [x, y, piece] = (
  read x :: X,
  read y :: Y,
  read piece :: Piece
  )
