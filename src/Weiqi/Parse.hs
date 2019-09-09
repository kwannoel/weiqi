module Weiqi.Parse where

import qualified Text.Read as R
import Data.Maybe

import Weiqi.Validate
import Weiqi.Types

parse :: String -> Maybe (Y, X, Piece)
parse input | verify input = Just . convert . words $ input
            | otherwise = Nothing

convert :: [String] -> (Y, X, Piece)
convert [y, x, piece] = (
  read y :: Y,
  read x :: X,
  read piece :: Piece
  )
