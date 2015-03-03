module DailyProgrammer.MetroTile where

import Data.Tree.RBTree (RBTree(Node, Leaf), insertOrd, deleteOrd, searchOrd, emptyRB)
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Data.Foldable as F

main = do
    bounds <- getLine
    chars <- getContents
    return ()

data Window = Window { title  :: Char
                     , xpos   :: Int
                     , ypos   :: Int
                     , width  :: Int
                     , height :: Int
                     } deriving (Show)

-- "Equal" windows have the same title and are overlapping or adjacent.
-- Otherwise the windows are compared by their title, x position, or y position
-- in that order.
-- (<) and (>) are transitive, but EQUALITY IS NOT. This could lead to some
-- weirdness, so we need to make sure that we don't insert two equal windows
-- into a tree.
instance Eq Window where v == w = (v <= w) && (w <= v)
instance Ord Window where
    Window t1 x1 y1 w1 h1 <= Window t2 x2 y2 w2 h2
        | t1 < t2                           = True
        | x1 < x2                           = True
        | y1 < y2                           = True
        | (x1 <= x2 + w2) && (y1 < y2 + h2) = True -- Equal (overlap or to right)
        | (y1 <= y2 + h2) && (x1 < x2 + w2) = True -- Equal (overlap or below)
        | otherwise                         = False

addPixel :: RBTree Window -> Window -> RBTree Window
wins `addPixel` pix 
    | isNothing existing = wins `insertOrd` pix
    | isJust    existing = (wins `deleteOrd` (Window t x y w h)) `insertOrd`
                           Window t x y (max w (xpos pix - x + 1))
                                        (max h (ypos pix - y + 1))
    where existing = wins `searchOrd` pix
          Window t x y w h = fromJust existing

instance F.Foldable RBTree where
foldr f z Leaf = z
foldr f z (Node _ v l r) = F.foldr f (f v (F.foldr f z r)) l

