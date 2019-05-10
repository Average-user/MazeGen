import Data.Array
import Solver
import System.Random
import Utils
import Data.Set (fromList)
import Test.QuickCheck

-- | Algorithms implemented so far
import qualified Algorithm.HuntKill
import qualified Algorithm.Sidewinder
import qualified Algorithm.Prims
import qualified Algorithm.GrowingTree
import qualified Algorithm.Backtracker

implies :: Bool -> Bool -> Bool
implies a b = not a || b 

perfectMaze :: StdGen -> ((Int,Int) -> StdGen -> Graph) -> (Int, Int) -> Bool
perfectMaze g algorithm (n,m) =
  (m > 0 && n > 0) `implies` (length (connected (algorithm (n,m) g)) == 1)

main :: IO ()
main = do
  g <- getStdGen
  quickCheck (perfectMaze g Algorithm.HuntKill.generate)
  quickCheck (perfectMaze g Algorithm.Sidewinder.generate)
  quickCheck (perfectMaze g Algorithm.Prims.generate)
  quickCheck (perfectMaze g Algorithm.GrowingTree.generate)
  quickCheck (perfectMaze g Algorithm.Backtracker.generate)
