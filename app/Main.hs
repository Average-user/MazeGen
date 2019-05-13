{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment (getScreenSize)
import           Solver
import           System.Random (getStdGen, StdGen)
import           Utils
import           ParseConfig
import qualified Data.ByteString.Lazy as B
import           Data.Map.Strict ((!),  keys)

-- | Algorithms implemented so far
import qualified Algorithm.HuntKill
import qualified Algorithm.Sidewinder
import qualified Algorithm.Prims
import qualified Algorithm.GrowingTree
import qualified Algorithm.Backtracker
import qualified Algorithm.Kruskals
import qualified Algorithm.Ellers

getAlgorithm :: Algorithm -> ((Int, Int) -> StdGen -> Graph)
getAlgorithm Backtracker = Algorithm.Backtracker.generate
getAlgorithm Prims       = Algorithm.Prims.generate
getAlgorithm HuntKill    = Algorithm.HuntKill.generate
getAlgorithm Sidewinder  = Algorithm.Sidewinder.generate
getAlgorithm GrowingTree = Algorithm.GrowingTree.generate
getAlgorithm Kruskals    = Algorithm.Kruskals.generate
getAlgorithm Ellers      = Algorithm.Ellers.generate

-- | Draw walls to de the adjacent cell of (x,y) that are not connected with it
drawWalls ::
  ((Int,Int) -> Point) {- ^ a function to scale a Coord -} ->
  Graph                {- ^ The maze                    -} ->
  (Int, Int)           {- ^ The Coordinate (x,y)        -} ->
  Picture              {- ^ The walls of course         -}
drawWalls f graph (x,y) = Pictures $ catMaybes fr'
  where
    ns  = graph ! (x, y)
    fr  = map ((,) <*> flip notElem ns) [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]
    fr' = map (\(v,b) -> if b then Just (close (x,y) v) else Nothing) fr
    close (x,y) (j,k) | y == k && j > x = line [f (j,k), f (j,k+1)]
                      | y == k          = line [f (x,y), f (x,y+1)]
                      | k > y           = line [f (j,k), f (j+1,k)]
                      | otherwise       = line [f (x,y), f (x+1,y)]

-- | Fills a cell width given color
square ::
  (Point -> Point) {- ^ Function scale point -} ->
  Point            {- ^ Coordinate           -} ->
  Color            {- ^ Color to fill with   -} ->
  Picture          {- ^ Square itself        -}
square f (x,y) c =
  Color c (Polygon (f <$> [(x,y), (x+1,y), (x+1,y+1), (x,y+1), (x,y)]))

c1 :: Num a => a
c1 = 5

-- | Decides what should be the constant to amplify stuff
-- | given the widht and height of the screen and maze sizes
getConstant :: Float -> Float -> Float -> Float -> Float
getConstant w h n m = min ((w-c1-c1) / n) ((h-c1-c1) / m)

main :: IO ()
main = do
  gen     <- getStdGen
  (w',h') <- getScreenSize
  configF <- B.readFile "config.json"
  case parseConfig configF of
    Left errorMsg -> putStrLn errorMsg
    Right (Config alg (n,m) r p (sx,sy) (ex,ey)) ->
      do let (w,h)      = (fromIntegral w', fromIntegral h')
             s c (x,y)  = ((x*c) - (w/2) + c1, (h/2) - c1 - (y*c))
             s' c (x,y) = s c (fromIntegral x, fromIntegral y)
             c          = getConstant w h (fromIntegral n) (fromIntegral m)
             hc         = c/2
             !graph     = nonPerfect (getAlgorithm alg) r (n,m) gen
             grid       = Pictures $ map (drawWalls (s' c) graph) (keys graph)
             pathsC     = paths graph (sx,sy) (ex,ey)
             minL       = minimum (map length pathsC)
             shortOnes  = filter ((==minL) . length) pathsC
             largeOnes  = filter ((/=minL) . length) pathsC
             sols       = map (Color violet . line . map (f 0 . s' c)) largeOnes
             goodSols   = map (Color red . line . map (f 0 . s' c)) shortOnes
             f z (x,y)  = (x + hc + z, y - hc + z)
             startp     = square (s c) (fromIntegral sx, fromIntegral sy) red
             goalp      = square (s c) (fromIntegral ex, fromIntegral ey) red
         display FullScreen white (if p
                                    then Pictures (sols ++ goodSols ++ [startp, goalp, grid])
                                    else grid)
