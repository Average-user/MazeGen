import Criterion.Main
import System.Random

-- | Algorithms implemented so far
import qualified Algorithm.HuntKill
import qualified Algorithm.Sidewinder
import qualified Algorithm.Prims
import qualified Algorithm.GrowingTree
import qualified Algorithm.Backtracker
import qualified Algorithm.Kruskals
import qualified Algorithm.Ellers

-- | To alwasy bench with the same StdGen
g :: StdGen
g = read "1896622768 1"

main :: IO ()
main = defaultMain [
      bgroup "50x50" [
            bench "HuntKill"    $ nf (flip Algorithm.HuntKill.generate g) (50,50)
          , bench "Sidewinder"  $ nf (flip Algorithm.Sidewinder.generate g) (50,50)
          , bench "Prims"       $ nf (flip Algorithm.Prims.generate g) (50,50)
          , bench "GrowingTree" $ nf (flip Algorithm.GrowingTree.generate g) (50,50)
          , bench "Backtracker" $ nf (flip Algorithm.Backtracker.generate g) (50,50)
          , bench "Kruskals"    $ nf (flip Algorithm.Kruskals.generate g) (50,50)
          , bench "Ellers"      $ nf (flip Algorithm.Ellers.generate g) (50,50)
          ]
    , bgroup "100x100" [
            bench "HuntKill"    $ nf (flip Algorithm.HuntKill.generate g) (100,100)
          , bench "Sidewinder"  $ nf (flip Algorithm.Sidewinder.generate g) (100,100)
          , bench "Prims"       $ nf (flip Algorithm.Prims.generate g) (100,100)
          , bench "GrowingTree" $ nf (flip Algorithm.GrowingTree.generate g) (100,100)
          , bench "Backtracker" $ nf (flip Algorithm.Backtracker.generate g) (100,100)
          , bench "Kruskals"    $ nf (flip Algorithm.Kruskals.generate g) (100,100)
          , bench "Ellers"      $ nf (flip Algorithm.Ellers.generate g) (100,100)
          ]
    ]
