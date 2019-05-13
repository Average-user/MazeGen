{-# LANGUAGE OverloadedStrings #-}

module ParseConfig where

import Safe
import Data.Aeson
import Data.Aeson.Types
import Data.Text as T
import Control.Monad
import Data.ByteString.Lazy

data Algorithm = Backtracker
               | Prims
               | HuntKill
               | GrowingTree
               | Sidewinder
               | Kruskals
               | Ellers deriving (Show, Read)

data Config = Config Algorithm (Int,Int) Int Bool (Int,Int) (Int,Int) deriving Show

instance FromJSON Config where
  parseJSON (Object o) = do
    let readField :: (Read a) => T.Text -> Parser a
        readField f = do
            v <- o .: f
            case readMay (T.unpack v) of
                Nothing -> fail (T.unpack f :: String)
                Just r  -> return r
    Config <$> readField "algorithm"
           <*> readField "maze-size"
           <*> readField "walls-to-remove"
           <*> readField "draw-paths"
           <*> readField "path-starting-point"
           <*> readField "path-ending-point"
  parseJSON _ = mzero

validCoord :: (Int, Int) -> (Int, Int) -> Bool
validCoord (n,m) (x,y) = and [x >= 0, y >= 0, x < n, y < m] 

checkConfig :: Config -> Either String Config
checkConfig (Config alg (n,m) r p (sx, sy) (ex, ey))
  | n < 0 || m < 0                      = Left "Error: Impossible to have maze of negative size"
  | r >= n*m                            = Left "Error: More walls to remove than walls in the maze"
  | r < 0                               = Left "Error: impossible to remove negative walls"
  | not (validCoord (n,m) (sx,sy)) && p = Left "Error: Starting point outside maze"
  | not (validCoord (n,m) (ex,ey)) && p = Left "Error: Ending point outside maze"
  | otherwise                           = Right $ Config alg (n,m) r p (sx,sy) (ex,ey) 

parseConfig :: ByteString -> Either String Config
parseConfig file = eitherDecode file >>= checkConfig
