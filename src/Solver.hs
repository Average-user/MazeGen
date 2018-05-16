module Solver where

import           Data.List (find)
import           Data.Map (Map, (!))
import           Data.Maybe (mapMaybe)
import           Data.Set (Set, toList, delete)
import qualified Data.Set as S
import qualified Data.Map as M

path :: Ord a =>  Map a (Set a) -> a -> a -> Maybe [a]
path graph s e = find (const True) $ mapMaybe (dfs [s]) $ toList (graph ! s)  
  where
    dfs (p:acc) n | n == e    = Just (n:p:acc)
                  | otherwise = find (const True) ops
      where
        ops = mapMaybe (dfs (n:p:acc)) $ toList $ delete p $ graph ! n
