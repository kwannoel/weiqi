module DynamicsSpec where

import Test.Hspec
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Map as M

import Weiqi.Dynamics
import Weiqi.Dynamics.Internal
import Weiqi.Board
import Weiqi.Types

spec :: Spec
spec = do
  describe "Weiqi.Dynamics" $ do
    
    it "Inserts a Weiqi piece" $ do
      board <- execStateT (placePiece (3, 3) Black) board9x9
      M.lookup (3, 3) board `shouldBe` Just Black

    it "Removes a Weiqi piece" $ do
      board <- flowerBoard
      M.lookup (4, 3) board `shouldBe` Just Black
      M.lookup (4, 5) board `shouldBe` Just Black
      let newBoard = removeSinglePiece (4, 3) board
      M.lookup (4, 3) newBoard `shouldBe` Just Empty
      let newBoard2 = removeSinglePiece (4, 5) newBoard
      M.lookup (4, 3) newBoard2 `shouldBe` Just Empty
      M.lookup (4, 5) newBoard2 `shouldBe` Just Empty

    it "Removes a list of pieces" $ do
      board <- flowerBoard
      M.lookup (4, 3) board `shouldBe` Just Black
      M.lookup (4, 5) board `shouldBe` Just Black
      M.lookup (3, 4) board `shouldBe` Just Black
      let newBoard = removePieces [(4, 3), (4, 5), (3, 4)] board
      M.lookup (4, 3) newBoard `shouldBe` Just Empty
      M.lookup (4, 5) newBoard `shouldBe` Just Empty
      M.lookup (3, 4) newBoard `shouldBe` Just Empty

    it "Gets Adjacent pieces" $ do
      adjacentCoords (1, 1) `shouldBe` [(2, 1), (0, 1), (1, 2), (1, 0)]

    it "filters pieces which are unchecked" $ do
      isNotChecked [(1, 2), (0, 1), (3, 4)] (2, 3) `shouldBe` True
      isNotChecked [(1, 2), (0, 1), (3, 4)] (1, 2) `shouldBe` False

    it "filters pieces which are the same" $ do
      board <- flowerBoard
      isSame Black board (4, 3) `shouldBe` True
      isSame Black board (0, 3) `shouldBe` False
      isSame Black board (4, 4) `shouldBe` False
      
    it "Checks a piece for no qi" $ do
      board <- noQi
      M.lookup (5, 4) board `shouldBe` Just Black
      M.lookup (4, 4) board `shouldBe` Just White
      checkFour [(4, 4)] White board `shouldBe` Dead [(4, 4)]      

flowerBoard :: IO Board
flowerBoard =
  flip execStateT board9x9 $ do
  flowerState

flowerState :: BoardState ()
flowerState = do
  board <- get
  placePiece (4, 3) Black 
  placePiece (4, 5) Black
  placePiece (3, 4) Black

noQi :: IO Board
noQi =
  flip execStateT board9x9 $ do
    flowerState
    placePiece (5, 4) Black
    board <- get
    put $ insertPieceAtCoord (4, 4) White board
