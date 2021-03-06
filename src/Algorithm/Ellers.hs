module Algorithm.Ellers where

import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import System.Random
import Utils

randBool :: StdGen -> (Bool, StdGen)
randBool g = let (i,g') = randomR ((0,1) :: Coord) g in (i == 1, g')

pickAtLeast1 :: StdGen -> [a] -> ([a], StdGen)
pickAtLeast1 g xs = if null chosen then ([xs!!index], g'') else (chosen, g')
  where
    (chosen, g')   = f xs g []
    (index, g'')   = randomR (0, length xs - 1) g'
    f []     g acc = (acc, g)
    f (x:xs) g acc = if b then f xs g' (x:acc) else f xs g' acc
      where
        (b, g') = randBool g

connectRight :: StdGen -> Bool -> Int -> Int -> M.Map Int Int -> Graph -> (M.Map Int Int, Graph, StdGen)
connectRight g lastRow n y prevI graph = f row g ident graph
  where
    row   = [0..n-1]
    ident = M.union prevI (M.fromList (zip row [n*y, n*y + 1 .. n*(y+1)-1]))
    f [a]      gen i graph  = (i, graph, g)
    f (a:b:rs) gen i graph
      | lastRow && ia /= ib = f (b:rs) gen' i' (connect graph (a,y) (b,y)) 
      | not c || (ia == ib) = f (b:rs) gen' i  graph
      | otherwise           = f (b:rs) gen' i' (connect graph (a,y) (b,y))
      where
        (c, gen') = randBool gen
        (ia, ib)  = (i M.! a, i M.! b)
        i'        = M.map (\n -> if n == ib then ia else n) i

connectBottom :: StdGen -> Int -> Int -> M.Map Int Int -> Graph -> (M.Map Int Int, Graph, StdGen)
connectBottom g n y prevI graph = f g sets M.empty graph
  where
    prevIL   = M.toList prevI
    ks       = M.keys prevI
    sets     = map org $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) prevIL
    org ((a,k):xs) = (k, a : map fst xs)
    f g []          ident graph = (ident, graph, g)
    f g ((k,cs):xs) ident graph = f g' xs newIdent newGraph
      where
        (cs', g') = pickAtLeast1 g cs
        newIdent  = foldl (\m x -> M.insert x k m) ident cs'
        newGraph  = foldl (\g x -> connect g (x,y) (x,y+1)) graph cs'

generate :: Coord -> StdGen -> Graph
generate (n,m) g = f g n 0 M.empty (start n m)
  where
    f g n y ident graph
      | y == m -1 = let (_,newGraph,_) = connectRight g True n y ident graph
                      in newGraph
      | otherwise = f g'' n (y+1) ident'' graph''                  
      where
        (ident', graph', g')    = connectRight g False n y ident graph
        (ident'', graph'', g'') = connectBottom g' n y ident' graph'
