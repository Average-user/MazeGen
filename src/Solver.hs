module Solver where

import qualified Data.Set as S
import Data.Map.Strict (Map, (!))

paths :: Ord a => Map a [a] -> a -> a -> [[a]]
paths graph s e = dfs (S.singleton s) [s] =<< graph ! s
  where
    dfs seen (p:acc) n
      | n == e          = [n:p:acc]
      | S.member n seen = []
      | otherwise       = dfs (S.insert n seen) (n:p:acc) =<< graph ! n
