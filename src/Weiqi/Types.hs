module Weiqi.Types where

import qualified Data.Map.Strict as M

type Board = M.Map (X, Y) PieceInfo

type X = Int
type Y = Int

data PieceInfo = PieceInfo Piece | Empty deriving (Eq, Show)

data Piece = Piece { colour :: Colour, connected :: [(X, Y)] } deriving (Eq, Show)

data Colour = Black | White deriving (Eq, Show)

type Qi = Int

type Valid = Bool
