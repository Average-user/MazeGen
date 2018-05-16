module Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow ((***))
import System.Random (StdGen, randomR)

type Coord = (Int, Int)
type Graph = M.Map Coord (S.Set Coord)

start :: Int -> Int -> Graph
start n m = M.fromList [((x,y), S.empty) | x <- [0..n-1], y <- [0..m-1]]

neighbors :: Coord -> Coord -> [Coord]
neighbors (n,m) (x,y) = filter f $ map ((+) x *** (+) y) [(1,0),(0,1),(-1,0),(0,-1)]
  where
    f (x,y) = not $ x < 0 || y < 0 || x >= n || y >= m

connect :: Graph -> Coord -> Coord -> Graph
connect g a b = M.adjust (S.insert b) a $ M.adjust (S.insert a) b g

sample :: [a] -> StdGen -> (a, StdGen)
sample xs g = (xs!!i, ng)
  where
    (i, ng) = randomR (0, length xs - 1) g

connected :: Ord a => M.Map a (S.Set a) -> [S.Set a] 
connected g = r (M.keys g) S.empty 
  where
    r []     _ = [] 
    r (x:xs) v = if x `S.member` v then r xs v else s xs v [x] S.empty 
    s xs v    []  w                  = w : r xs (v `S.union` w) 
    s xs v (y:ys) w | y `S.member` w = s xs v ys w
                    | otherwise      = s xs v (ys ++ (S.toList (g M.! y))) (S.insert y w)
