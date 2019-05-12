module Algorithm.Backtracker where

import Utils
import qualified Data.Set as S
import System.Random (StdGen, randomR)

generate :: (Int,Int) -> StdGen -> Graph
generate (n,m) g = r (start n m) (S.singleton (x,y)) [(x,y)] (x,y) g1
  where
    (x, g1) = randomR (0, n-1) g
    (y, g2) = randomR (0, m-1) g1
    r graph _    [] _ _ = graph
    r graph seen xs c g
      | null ns   = r graph seen (tail xs) (head xs) g
      | otherwise = r (connect graph nc c) (S.insert nc seen) (c:xs) nc ng
      where
        ns       = filter (`S.notMember` seen) $ neighbors (n,m) c
        (nc, ng) = sample ns g
