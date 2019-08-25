module Weiqi.Render where

import Data.Map.Strict as M
import Weiqi.Types

renderPieceInfo :: PieceInfo -> String
renderPieceInfo p =
  case p of
    Empty -> "."
    PieceInfo piece -> renderPiece piece

renderPiece :: Piece -> String
renderPiece p =
  case colour p of
    Black -> "x"
    White -> "o"

renderBoard :: Board -> String
renderBoard board = M.foldlWithKey' accumulateBoard "" piecesAsStrings
  where piecesAsStrings = fmap renderPieceInfo board

accumulateBoard :: String -> (X, Y) -> String -> String
accumulateBoard accum c@(_, y) piece | y == 0 = accum ++ "\n" ++ piece
                                     | otherwise = accum ++ piece
