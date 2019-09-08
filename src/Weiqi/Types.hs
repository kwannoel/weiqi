module Weiqi.Types where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State

type BoardState = StateT Board IO

type Error = String
type Board = M.Map (X, Y) Piece

type X = Int
type Y = Int

data Piece = Black | White | Empty deriving (Eq, Show, Read)

data Action = Remove | Place deriving (Eq, Show, Read)

data Qi = Alive | Dead [(X, Y)] deriving Eq

type Valid = Bool
