module Algorithm.GrowingTree where

import Utils
import qualified Data.Set as S
import System.Random (StdGen, randomR)
import Data.List (delete)

generate :: (Int, Int) -> StdGen -> Graph
generate (n,m) g = r (start n m) [(sx,sy)] (S.singleton (sx,sy)) (sx,sy) g2
  where
    (sx, g1) = randomR (0, n-1) g
    (sy, g2) = randomR (0, m-1) g1
    r graph c seen coord g
      | null c    = graph
      | null ns   = r graph (delete coord c) seen (head c) g
      | otherwise = r ngraph (nc : c) (S.insert nc seen) nc ng
      where 
        ns       = filter (`S.notMember` seen) $ neighbors (n,m) coord
        (nc, ng) = sample ns g
        ngraph   = connect graph coord nc
  
