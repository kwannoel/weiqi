module Weiqi.Game where

import System.IO
import Control.Monad.IO.Class
import qualified Text.Read as R
import Control.Monad.Trans.State

import Weiqi.Board
import Weiqi.Render
import Weiqi.Dynamics
import Weiqi.Validate
import Weiqi.Parse
import Weiqi.Render
import Weiqi.Types

runWeiqiDefault :: IO ()
runWeiqiDefault =
  evalStateT runWeiqi board9x9

runWeiqi :: BoardState ()
runWeiqi = do
  board <- get
  liftIO . putStrLn $ renderBoard board
  liftIO . putStrLn $ "Enter your input in the form of: \"<y> <x> <colour>\""
  input <- liftIO getLine
  case (parse input) of
    Just parsedInput -> execute parsedInput
    Nothing -> (liftIO . putStrLn $ "Invalid Input")
  runWeiqi

execute :: (X, Y, Piece) -> BoardState ()
execute (x, y, piece) = placePiece (x, y) $ piece
