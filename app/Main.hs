{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Time.Clock
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment (getScreenSize)
import           Solver
import           System.Environment (getArgs)
import           System.Random (getStdGen, StdGen)
import           Utils
import           Data.List ((\\))

-- | Algorithms implemented so far
import qualified HuntKill
import qualified Sidewinder
import qualified Prims
import qualified GrowingTree
import qualified Backtracker

pickAlgorithm :: Int -> (Int -> (Int,Int) -> StdGen -> Graph)
pickAlgorithm n = case n of
                    1 -> nonPerfect Sidewinder.generate
                    2 -> nonPerfect Prims.generate
                    3 -> nonPerfect GrowingTree.generate
                    4 -> nonPerfect HuntKill.generate
                    5 -> nonPerfect Backtracker.generate

-- | Draw walls to de the adjacent cell of (x,y) that are not connected with it
drawWalls ::
  ((Int,Int) -> Point) {- ^ a function to scale a Coord -} ->
  Graph                {- ^ The maze                    -} ->
  (Int, Int)           {- ^ The Coordinate (x,y)        -} ->
  Picture              {- ^ The walls of course         -}
drawWalls f graph (x,y) = Pictures $ catMaybes fr'
  where
    ns  = graph M.! (x, y)
    fr  = map ((,) <*> flip S.notMember ns) [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]
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
  args    <- map read <$> getArgs
  (w',h') <- getScreenSize
  let (w,h)      = (fromIntegral w', fromIntegral h')
      s c (x,y)  = ((x*c) - (w/2) + c1, (h/2) - c1 - (y*c))
      s' c (x,y) = s c (fromIntegral x, fromIntegral y)
      [p,r,a,n,m]= args
      c          = getConstant w h (fromIntegral n) (fromIntegral m)
      hc         = c/2
      !graph     = pickAlgorithm a r (n,m) gen
      grid       = Pictures $ map (drawWalls (s' c) graph) (M.keys graph)
      pathsC     = paths graph (0,0) (n-1,m-1)
      minL       = minimum (map length pathsC)
      shortOnes  = filter ((==minL) . length) pathsC
      largeOnes  = filter ((/=minL) . length) pathsC
      sols       = map (Color violet . line . map (f 0 . s' c)) largeOnes
      goodSols   = map (Color red . line . map (f 0 . s' c)) shortOnes
      f z (x,y)  = (x + hc + z, y - hc + z)
      startp     = square (s c) (0,0) red
      goalp      = square (s c) (fromIntegral (n-1), fromIntegral (m-1)) red
  display FullScreen white (if p == 1
                             then Pictures (sols ++ goodSols ++ [startp, goalp, grid])
                             else grid)
