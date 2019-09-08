module Weiqi.Dynamics.Validate where

import qualified Data.Map.Strict as M
import Weiqi.Types

checkValid :: Piece -> (X, Y) -> Board -> Valid
checkValid piece coord board =
  case M.lookup coord board of
    Just Empty -> True
    _ -> False
