module BoardSpec where

import Test.Hspec

import Data.Map as M
import Weiqi.Board
import Weiqi.Types

spec :: Spec
spec = do
  describe "Weiqi.Board" $ do
    it "returns a 3x3 Board" $ do
      emptyBoard [0..2] [0..2] `shouldBe` M.fromList [ ((0, 0), Empty), ((0, 1), Empty), ((0, 2), Empty)
                                                     , ((1, 0), Empty), ((1, 1), Empty), ((1, 2), Empty)
                                                     , ((2, 0), Empty), ((2, 1), Empty), ((2, 2), Empty)
                                                     ]
