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
  (m > 0 && n > 0) `implies` (length (connected graph) == 1 && acyclic graph)
  where
    graph = algorithm (n,m) g

nonPerfectMaze :: StdGen -> ((Int,Int) -> StdGen -> Graph) -> (Int, Int) -> Int -> Bool
nonPerfectMaze g algorithm (n,m) walls =
  (m > 1 && n > 1 && walls > 0 && walls < n*m)
  `implies`
  (length (connected graph) == 1 && not (acyclic graph))
  where
    graph = nonPerfect algorithm walls (n,m) g

testAlgorithm :: String -> ((Int,Int) -> StdGen -> Graph) -> IO ()
testAlgorithm s alg = do
  g <- getStdGen
  timeIO s (do quickCheck (perfectMaze g alg)
               quickCheck (nonPerfectMaze g alg))

main :: IO ()
main = do
  testAlgorithm "\nTesting Huntkill: \n"    Algorithm.HuntKill.generate
  testAlgorithm "Testing Sidewinder: \n"  Algorithm.HuntKill.generate
  testAlgorithm "Testing Prims: \n"       Algorithm.HuntKill.generate
  testAlgorithm "Testing GrowingTree: \n" Algorithm.HuntKill.generate
  testAlgorithm "Testing Backtracker: \n" Algorithm.HuntKill.generate
  testAlgorithm "Testing Kruskals: \n"    Algorithm.HuntKill.generate

timeIO :: String -> IO () -> IO ()
timeIO msg f = do
  putStr msg
  start <- getCurrentTime
  f
  stop <- getCurrentTime
  print $ diffUTCTime stop start
  putStr "\n"
