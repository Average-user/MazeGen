module Algorithm.Sidewinder where

import System.Random (StdGen)
import Utils

firstRow :: (Int, Int) -> Graph -> Graph
firstRow (n,m) g =
  foldr (\(x,y) g  -> connect g (x,y) (x+1,y)) g $ zip [0..n-2] (repeat 0)

pickRand :: [Coord] -> Graph -> StdGen -> (Graph, StdGen)
pickRand r graph g = (connect graph (x,y) (x,y-1), ng)
  where
    ((x,y),ng) = sample r g

row :: (Int,Int) -> Int -> Int -> Graph -> [Coord] -> StdGen -> (Graph, StdGen)
row (n,m) x y graph r g
  | x == (n-1) = pickRand ((x,y):r) graph g
  | b          = row (n,m) (x+1) y (connect graph (x,y) (x+1,y)) ((x,y):r) ga
  | otherwise  = row (n,m) (x+1) y ng [] gb
  where
    (b,  ga) = sample [True,False] g
    (ng, gb) = pickRand ((x,y):r) graph ga

generate :: (Int,Int) -> StdGen -> Graph
generate (n,m) g = fst $ foldr (\y (gr,g) -> row (n,m) 0 y gr [] g)
                               (firstRow (n,m) (start n m), g)
                               [1..m-1]
