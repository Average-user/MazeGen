module Algorithm.Kruskals where

import Utils
import System.Random
import qualified Data.IntDisjointSet as ID
import Data.Array

genEdges :: Coord -> [(Coord, Coord)]
genEdges (n,m) =
  [(x,y) | x <- [0..n-1], y <- [0..m-1]] >>= \e -> map ((,) e) (neighbors (n,m) e)

createSets :: Coord -> (ID.IntDisjointSet, Array Coord Int)
createSets (n,m) =
  (ID.fromList [(a,a) | a <- [0..n*m-1]] , listArray ((0,0), (n-1,m-1)) [0..])
    
generate :: (Int, Int) -> StdGen -> Graph
generate (n,m) g = f edges (start n m) sets
  where
    edges        = fst (shuffle (genEdges (n,m)) g)
    (sets, maps) = createSets (n,m)
    f []         graph _               = graph
    f ((a,b):es) graph sets
      | fst (ID.equivalent ia ib sets) = f es graph sets
      | otherwise                      = f es (connect graph a b) (ID.union ia ib sets)
      where
        ia = maps ! a
        ib = maps ! b
