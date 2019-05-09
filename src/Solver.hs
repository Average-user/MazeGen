module Solver where

import           Data.List (find)
import           Data.Map (Map, (!))
import           Data.Maybe (mapMaybe)
import           Data.Set (Set, toList, delete)
import qualified Data.Set as S
import qualified Data.Map as M

paths :: Ord a => Map a (Set a) -> a -> a -> [[a]]
paths graph s e = dfs (S.singleton s) [s] =<< graph' ! s
  where
    graph' = M.map toList graph
    dfs seen (p:acc) n
      | n == e          = [n:p:acc]
      | S.member n seen = []
      | otherwise       = dfs (S.insert n seen) (n:p:acc) =<< graph' ! n
