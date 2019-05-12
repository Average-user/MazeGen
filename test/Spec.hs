import Data.Array
import Solver
import System.Random
import Utils
import Data.Set (fromList)
import Test.QuickCheck
import Data.Time

-- | Algorithms implemented so far
import qualified Algorithm.HuntKill
import qualified Algorithm.Sidewinder
import qualified Algorithm.Prims
import qualified Algorithm.GrowingTree
import qualified Algorithm.Backtracker
import qualified Algorithm.Kruskals

implies :: Bool -> Bool -> Bool
implies a b = not a || b 

perfectMaze :: StdGen -> ((Int,Int) -> StdGen -> Graph) -> (Int, Int) -> Bool
perfectMaze g algorithm (n,m) =
  (m > 0 && n > 0) `implies` (length (connected (algorithm (n,m) g)) == 1)

main :: IO ()
main = do
  g <- getStdGen
  timeIO "Huntkill test: "    $ quickCheck (perfectMaze g Algorithm.HuntKill.generate)
  timeIO "Sidewinder test: "  $ quickCheck (perfectMaze g Algorithm.Sidewinder.generate)
  timeIO "Prims test: "       $ quickCheck (perfectMaze g Algorithm.Prims.generate)
  timeIO "GrowingTree test: " $ quickCheck (perfectMaze g Algorithm.GrowingTree.generate)
  timeIO "Backtracker test: " $ quickCheck (perfectMaze g Algorithm.Backtracker.generate)
  timeIO "Kruskals test: "    $ quickCheck (perfectMaze g Algorithm.Kruskals.generate)

timeIO :: String -> IO () -> IO ()
timeIO msg f = do
  putStr msg
  start <- getCurrentTime
  f
  stop <- getCurrentTime
  print $ diffUTCTime stop start
