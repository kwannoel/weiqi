module Main where

import Weiqi.Board
import Weiqi.Render
import Weiqi.Types
import System.IO

runWeiqi :: board -> IO ()
runWeiqi board = 
  putStrLn $ renderBoard board9x9
  putStrLn "Enter your input in the form of: \"<y> <x> <colour>\""
  input <- hGetLine stdin
  case length (words input) of
    3 -> let [y, x, colour] = words input
             piece = Piece (read colour :: Colour) []
         in  insertPiece (read x :: X, read y :: Y) piece (renderBoard board9x9)
    _ -> putStrLn $ "Invalid input: " ++ show input
