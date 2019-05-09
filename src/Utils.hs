module Utils where

import Data.Array
import Control.Arrow ((***))
import System.Random
import qualified Data.Set as S
import qualified Data.Map as M

type Coord = (Int, Int)
type Graph = Array Coord [Coord]

start :: Int -> Int -> Graph
start n m = listArray ((0,0), (n-1,m-1)) [[] | x <- [0..n-1], y <- [0..m-1]]


neighbors :: Coord -> Coord -> [Coord]
neighbors (n,m) (x,y) = filter f $ map ((+) x *** (+) y) [(1,0),(0,1),(-1,0),(0,-1)]
  where
    f (x,y) = not $ x < 0 || y < 0 || x >= n || y >= m


connect :: Graph -> Coord -> Coord -> Graph
connect g a b = accum (\x y -> y : x) g [(a, b), (b, a)]

sample :: [a] -> StdGen -> (a, StdGen)
sample xs g = (xs!!i, ng)
  where
    (i, ng) = randomR (0, length xs - 1) g

{-
connected :: Ord a => M.Map a (S.Set a) -> [S.Set a] 
connected g = r (M.keys g) S.empty 
  where
    r []     _ = [] 
    r (x:xs) v = if x `S.member` v then r xs v else s xs v [x] S.empty 
    s xs v    []  w                  = w : r xs (v `S.union` w) 
    s xs v (y:ys) w | y `S.member` w = s xs v ys w
                    | otherwise      = s xs v (ys ++ S.toList (g M.! y)) (S.insert y w)
-}

closedWalls :: (Int,Int) -> Graph -> [(Coord, Coord)]
closedWalls (n,m) graph = S.toList (S.fromList all S.\\ S.fromList occupied)
  where
    all      = indices graph >>= \x ->  map ((,) x) (neighbors (n,m) x)
    occupied = assocs graph >>= \(k, ks) -> map ((,) k) ks


removeRandomWalls :: StdGen -> (Int, Int) -> Int -> Graph -> Graph
removeRandomWalls g (n,m) c graph = foldl (\g (a,b) -> connect g a b) graph chosen
  where
    options = closedWalls (n,m) graph
    chosen  = take c (fst (fisherYates g options))

fisherYatesStep :: (M.Map Int a, StdGen) -> (Int, a) -> (M.Map Int a, StdGen)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: StdGen -> [a] -> ([a], StdGen)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1..]
    initial x gen = (M.singleton 0 x, gen)

nonPerfect :: ((Int, Int) -> StdGen -> Graph) -> Int -> (Int,Int) -> StdGen -> Graph
nonPerfect perfect c (n,m) g = removeRandomWalls g (n,m) c (perfect (n,m) g)
