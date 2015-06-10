module DailyProgrammer.Chester where

import qualified Data.Trees.KdTree as KD
import Data.Maybe (fromJust)

data Point2d = Point2d { p2x :: Double, p2y :: Double }
    deriving (Eq, Ord, Show)

instance KD.Point Point2d where 
    dimension _ = 2
    coord 0 p = p2x p
    coord 1 p = p2y p

dist p q = sqrt $ KD.dist2 p q

parseTreat line = Point2d (coords!!0) (coords!!1)
    where coords = map read . words $ line

trip progress _ KD.KdEmpty = progress
trip progress pos treats = trip (progress + delta) nextTreat (KD.remove treats nextTreat)
    where nextTreat = fromJust $ KD.nearestNeighbor treats pos
          delta = dist pos nextTreat

main = do 
    numTreats <- getLine
    input <- getContents
    let treats = KD.fromList . map parseTreat . lines $ input
    print $ trip 0 (Point2d 0.5 0.5) treats

