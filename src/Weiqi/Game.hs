module Weiqi.Game where

import Weiqi.Board
import Weiqi.Render
import Weiqi.Dynamics
import Weiqi.Render
import Weiqi.Types
import System.IO
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
import qualified Text.Read as R
import Control.Monad.Trans.State

runWeiqiDefault :: IO ()
runWeiqiDefault =
  evalStateT runWeiqi board9x9

runWeiqi :: BoardState ()
runWeiqi = do
  board <- get
  liftIO . putStrLn $ renderBoard board
  liftIO . putStrLn $ "Enter your input in the form of: \"<y> <x> <colour> (Remove|Place)\""
  input <- liftIO getLine
  case (parse input) of
    Just parsedInput -> execute parsedInput
    Nothing -> (liftIO . putStrLn $ "Invalid Input length")
  runWeiqi

parse :: String -> Maybe (X, Y, Colour, Action)
parse input | verify input = Just . convert . words $ input
            | otherwise = Nothing

verify :: String -> Bool
verify input | length (words input) == 4 = validate (words input)
             | otherwise = False

validate :: [String] -> Bool
validate [x, y, colour, action] =
  not . getAll . fold . map All $
  [(R.readMaybe x :: Maybe X) == Nothing,
   (R.readMaybe y :: Maybe Y) == Nothing,
   (R.readMaybe colour :: Maybe Colour) == Nothing,
   (R.readMaybe action :: Maybe Action) == Nothing
  ]

convert :: [String] -> (X, Y, Colour, Action)
convert [x, y, colour, action] = (
  read x :: X,
  read y :: Y,
  read colour :: Colour,
  read action :: Action
  )

execute :: (X, Y, Colour, Action) -> BoardState ()
execute (x, y, colour, action) =
  case action of
    Remove -> removePiece (x, y)
    Place -> placePiece (x, y) $ Piece colour []
