module DailyProgrammer.MetroTile where

import Data.Tree.RBTree (RBTree(Node, Leaf), insertOrd, deleteOrd, searchOrd, emptyRB)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Monoid (mempty, mappend)
import Data.List (foldl')

main = do
    bounds <- getLine
    chars <- getContents
    let [xb, yb] = map read $ words bounds
    mapM_ print . toList . foldl' addPixel emptyRB . pixels xb yb . concat . lines $ chars

data Window = Window { title  :: Char
                     , xpos   :: Int
                     , ypos   :: Int
                     , width  :: Int
                     , height :: Int
                     }

-- "Equal" windows have the same title and are overlapping or adjacent.
-- Otherwise the windows are ordered by x position and y position in that order.
--
-- (<) and (>) are transitive, but EQUALITY IS NOT. This could lead to some
-- weirdness, so we need to make sure that we don't insert two equal windows
-- into the same tree.
instance Eq Window where v == w = v `compare` w == EQ

{-|
FIXME: This is hella broken because some pixels in window v may be less than
window w while some may be greater. In that situation the tree ordering is
fucked and you end up with duplicate windows in the RBTree.
Note to self: don't ever try to hack Ord like this again. I suspect it will
always come back to bite.

Solution approach n+1:
======================
Implement a legit window order and store the windows in a set. Sets are
implemented with balanced binary search trees, so the Window order and
performance should be preserved. I'll need to implement my own algorithm to
search for a window containing a pixel. I'm not sure what that will look like,
but I think it will have to be in linear time. Maybe use a zipper?
-}
instance Ord Window where
    Window t1 x1 y1 w1 h1 `compare` Window t2 x2 y2 w2 h2
        | (t1 == t2) && (x1 <= x2) && (x2 <= x1+w1) && (y1 <= y2) && (y2 <= y1+h1) = EQ
        | (t1 == t2) && (x2 <= x1) && (x1 <= x2+w2) && (y2 <= y1) && (y1 <= y2+h2) = EQ
        | y1 < y2 = LT
        | x1 < x2 = LT
        | otherwise = GT

instance Show Window where
    show (Window t x y w h) = show w ++ 'x' : show h ++ " tile of character '"
                              ++ t : "' located at (" ++ show x ++ ','
                              : show y ++ ")"

-- This shouldn't be necessary, but RBTree isn't an instance of Foldable
toList Leaf = []
toList (Node _ v l r) = toList l ++ v : toList r

-- Take an x-bound, y-bound, and list of characters and generate a list of
-- "pixels" represented by 1x1 windows.
pixels :: Int -> Int -> [Char] -> [Window]
pixels xb yb chars = map mkPix . filter ((/='.') . first) $ zipWith triple chars positions
    where positions      = [(x,y) | y <- [0..yb-1], x <- [0..xb-1]]
          triple a (b,c) = (a,b,c)
          first (x,_,_)  = x
          mkPix (c,x,y)  = Window {title=c, xpos=x, ypos=y, width=1, height=1}

-- Accumulation function for the RBTree of windows. Take a RBTree of Windows
-- and a pixel. Add the pixel to the RBTree by either expanding one of the
-- windows or adding a new window. The RBTree will keep everything balanced and
-- ordered.
addPixel :: RBTree Window -> Window -> RBTree Window
wins `addPixel` pix
    | isNothing existing = wins `insertOrd` pix
    | isJust    existing = (wins `deleteOrd` (Window t x y w h)) `insertOrd`
                           Window t x y (max w (xpos pix - x + 1))
                                        (max h (ypos pix - y + 1))
    where existing = wins `searchOrd` pix
          Window t x y w h = fromJust existing

