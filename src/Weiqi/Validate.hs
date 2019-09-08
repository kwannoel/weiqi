module Weiqi.Validate where

import Data.Foldable
import Data.Monoid
import Data.Maybe
import qualified Text.Read as R
import qualified Data.Map.Strict as M
import Weiqi.Types

verify :: String -> Bool
verify input | length (words input) == 3 = validate (words input)
             | otherwise = False

-- Update error handling
validate :: [String] -> Bool
validate [x, y, piece] =
  getAll . foldMap All $
  [isJust (R.readMaybe x :: Maybe X),
   isJust (R.readMaybe y :: Maybe Y),
   isJust (R.readMaybe piece :: Maybe Piece)
  ]

checkValid :: Piece -> (X, Y) -> Board -> Valid
checkValid piece coord board =
  case M.lookup coord board of
    Just Empty -> True
    _ -> False
