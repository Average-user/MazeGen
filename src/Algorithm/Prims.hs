module Algorithm.Prims where

import Utils

import Data.Set (Set, toList, delete, insert, member, fromList, singleton)
import System.Random (StdGen)

pickNode ::
  (Int, Int) ->
  Set Coord  ->
  Graph      ->
  Set Coord  ->
  StdGen     ->
  (Set Coord, Graph, Set Coord, StdGen)
pickNode (n,m) fr graph seen g =
  (delete nc nf, connect graph nc s', insert nc seen, ng')
  where
    (nc, ng)  = sample (toList fr) g
    ns        = neighbors (n,m) nc
    sn        = filter (`member` seen) ns
    nf        = foldr insert fr (filter (`notElem` sn) ns)
    (s', ng') = sample sn ng

generate :: (Int, Int) -> StdGen -> Graph
generate (n,m) g = let (_, graph, _, _) = iterate f s !!(n*m-1)
                     in graph
  where
    s = (fromList (neighbors (n,m) (0,0)), start n m, singleton (0,0), g)
    f (fr, graph, seen, g) = pickNode (n,m) fr graph seen g
    
