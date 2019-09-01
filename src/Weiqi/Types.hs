module Weiqi.Types where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State

type BoardState = State Board Representation

type Error = String
type Representation = String
type Board = M.Map (X, Y) PieceInfo

type X = Int
type Y = Int

data PieceInfo = PieceInfo Piece | Empty deriving (Eq, Show)

data Piece = Piece { colour :: Colour, connected :: [(X, Y)] } deriving (Eq, Show)

data Colour = Black | White deriving (Eq, Show, Read)

type Qi = Int

type Valid = Bool
