import Data.Map.Strict as M
import Weiqi.Board
import Weiqi.Render
import Weiqi.Types

main :: IO ()
main = do
  foldMap
    (putStrLn . show)
    [
      emptyBoard [0,1] [0,1] == M.fromList [((0,0), Empty), ((0,1), Empty),
                                            ((1,0), Empty), ((1,1), Empty)],
      renderBoard (emptyBoard [0,1] [0,1]) == "\n..\n..",
      emptyBoard [0..2] [0..2] == M.fromList [ ((0,0), Empty), ((0,1), Empty), ((0,2), Empty)
                                             , ((1,0), Empty), ((1,1), Empty), ((1,2), Empty)
                                             , ((2,0), Empty), ((2,1), Empty), ((2,2), Empty)
                                             ]]
  print $ renderBoard (emptyBoard [0,1] [0,1])
