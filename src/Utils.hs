module Utils where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef
import qualified Data.Set as S
import System.Random

type Coord = (Int, Int)
type Graph = Array Coord [Coord]

start :: Int -> Int -> Graph
start n m = listArray ((0,0), (n-1,m-1)) (repeat [])

neighbors :: Coord -> Coord -> [Coord]
neighbors (n,m) (x,y) = filter f $ map ((+) x *** (+) y) [(1,0),(0,1),(-1,0),(0,-1)]
  where
    f (x,y) = not $ x < 0 || y < 0 || x >= n || y >= m


connect :: Graph -> Coord -> Coord -> Graph
connect g a b = accum (flip (:)) g [(a, b), (b, a)]

sample :: [a] -> StdGen -> (a, StdGen)
sample xs g = (xs!!i, ng)
  where
    (i, ng) = randomR (0, length xs - 1) g

connected :: Ix a => Array a [a] -> [S.Set a] 
connected g = r (indices g) S.empty 
  where
    r []     _ = [] 
    r (x:xs) v = if x `S.member` v then r xs v else s xs v [x] S.empty 
    s xs v    []  w                  = w : r xs (v `S.union` w) 
    s xs v (y:ys) w | y `S.member` w = s xs v ys w
                    | otherwise      = s xs v (ys ++ (g ! y)) (S.insert y w)

acyclic :: Graph -> Bool
acyclic graph = f S.empty (-1,-1) (0,0)
  where
    f seen prev node = S.notMember node seen && all (f (S.insert node seen) node) newNodes
      where
        newNodes = filter (/=prev) (graph ! node)

closedWalls :: (Int,Int) -> Graph -> [(Coord, Coord)]
closedWalls (n,m) graph = S.toList (S.fromList all S.\\ S.fromList occupied)
  where
    all      = indices graph >>= \x ->  map ((,) x) (neighbors (n,m) x)
    occupied = assocs graph >>= \(k, ks) -> map ((,) k) ks


removeRandomWalls :: StdGen -> (Int, Int) -> Int -> Graph -> Graph
removeRandomWalls g (n,m) c graph = foldl (\g (a,b) -> connect g a b) graph chosen
  where
    options = closedWalls (n,m) graph
    chosen  = take c (fst (shuffle options g))

nonPerfect :: ((Int, Int) -> StdGen -> Graph) -> Int -> (Int,Int) -> StdGen -> Graph
nonPerfect perfect c (n,m) g = removeRandomWalls g (n,m) c (perfect (n,m) g)

shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs
