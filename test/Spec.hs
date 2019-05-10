import Data.Array
import Solver
import System.Random
import Utils
import Data.Set (fromList)
import Test.QuickCheck

-- | Algorithms implemented so far
import qualified HuntKill
import qualified Sidewinder
import qualified Prims
import qualified GrowingTree
import qualified Backtracker

implies :: Bool -> Bool -> Bool
implies a b = not a || b 

perfectMaze :: StdGen -> ((Int,Int) -> StdGen -> Graph) -> (Int, Int) -> Bool
perfectMaze g algorithm (n,m) =
  (m > 0 && n > 0) `implies` (length (connected (algorithm (n,m) g)) == 1)

main :: IO ()
main = do
  g <- getStdGen
  quickCheck (perfectMaze g HuntKill.generate)
  quickCheck (perfectMaze g Sidewinder.generate)
  quickCheck (perfectMaze g Prims.generate)
  quickCheck (perfectMaze g GrowingTree.generate)
  quickCheck (perfectMaze g Backtracker.generate)
