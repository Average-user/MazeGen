module Algorithm.HuntKill where

import Data.Set (Set, insert, notMember, member, singleton)
import System.Random (StdGen, randomR)
import Utils
import Data.Map.Strict (toList)

walk :: (Int, Int) -> Coord -> Graph -> Set Coord -> StdGen -> Graph
walk (n,m) coord graph seen g
  | null ns   = hunt (n,m) graph seen g
  | otherwise = walk (n,m) ncoord (connect graph coord ncoord) (insert ncoord seen) ng
  where
    ns     = filter (`notMember` seen) $ neighbors (n,m) coord
    (i,ng) = randomR (0, length ns - 1) g
    ncoord = ns !! i

hunt :: Coord -> Graph -> Set Coord -> StdGen -> Graph
hunt (n,m) graph seen g =
  case find (toList graph) of
    Just (s,c) -> walk (n,m) s (connect graph s c) (insert s seen) g
    Nothing    -> graph
  where
    find []                                               = Nothing
    find ((k,_):xs) | k `notMember` seen && not (null ns) = Just (k, head ns)
                    | otherwise                           = find xs
      where
        ns = filter (`member` seen) $ neighbors (n,m) k

generate :: Coord -> StdGen -> Graph
generate (n,m) gen = walk (n,m) (sx,sy) (start n m) (singleton (sx,sy)) g2
  where
    (sx, g1) = randomR (0, n-1) gen
    (sy, g2) = randomR (0, m-1) g1
