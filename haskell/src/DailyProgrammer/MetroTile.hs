module DailyProgrammer.MetroTile where

import Data.List (foldl')
import Control.Monad.State

data Window = Window { title  :: Char
                     , xpos   :: Int
                     , ypos   :: Int
                     , width  :: Int
                     , height :: Int
                     } deriving (Show)

mergableH (Window t1 x1 y1 w1 h1) (Window t2 x2 y2 w2 h2)
    | t1 /= t2      = False
    | y1 /= y2      = False
    | h1 /= h2      = False
    | x1 + w1 == x2 = True
    | x2 + w2 == x1 = True
    | otherwise     = False

mergableV (Window t1 x1 y1 w1 h1) (Window t2 x2 y2 w2 h2)
    | t1 /= t2      = False
    | x1 /= x2      = False
    | w1 /= w2      = False
    | y1 + h1 == y2 = True
    | y2 + h2 == y1 = True
    | otherwise     = False

merge v w
    | mergableV v w = Window (title v) (xpos v) (min (ypos v) (ypos w)) (width v) (height v + height w)
    | mergableH v w = Window (title v) (min (xpos v) (xpos w)) (ypos v) (width v + width w) (height v)
    | otherwise = error "Merged windows must be adjacent, have the same title, and have the same width or height."


pixels :: [Char] -> [(Int,Int)] -> [Window]
pixels cs ixs = filter (\w -> (title w /= '.')) (zipWith makeWin cs ixs)
    where makeWin c (x,y) = Window {title=c, xpos=x, ypos=y, width=1, height=1}

-- Combine a row of pixels horizontally where possible.
-- Return the resuling windows in reverse order by xpos.
concatH :: [Window] -> [Window]
concatH = foldl' acc []
    where acc [] v = [v]
          acc (w:ws) v | mergableH v w  = (merge v w):ws
                       | otherwise      = v:w:ws

-- Combine two rows of pixels vertically where possible.
-- Return the resulting windows in ascending order by xpos.
concatV :: [Window] -> [Window] -> [Window]
concatV []     (w:ws) = w:ws
concatV (v:vs) []     = v:vs
concatV (v:vs) (w:ws) | xpos v >  xpos w = v:(concatV vs (w:ws))
                      | xpos v <  xpos w = w:(concatV (v:vs) ws)
                      | mergableV v w    = (merge v w):(concatV vs ws)
                      | otherwise        = v:(concatV vs (w:ws))
