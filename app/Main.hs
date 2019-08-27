module Main where

import Weiqi.Board
import Weiqi.Render
import Weiqi.Dynamics
import Weiqi.Render
import Weiqi.Types
import System.IO

handleInsert :: Either String Board -> IO ()
handleInsert (Left str) = putStrLn str
handleInsert (Right board) = runWeiqi board

runWeiqi :: Board -> IO ()
runWeiqi board = do
  putStrLn $ renderBoard board
  putStrLn "Enter your input in the form of: \"<y> <x> <colour>\""
  input <- hGetLine stdin
  case length (words input) of
    3 -> let [y, x, colour] = words input
             piece = Piece (read colour :: Colour) []
         in  handleInsert $ insertPiece (read x :: X, read y :: Y) piece board
    _ -> putStrLn $ "Invalid input: " ++ show input

main :: IO ()
main = runWeiqi board9x9
