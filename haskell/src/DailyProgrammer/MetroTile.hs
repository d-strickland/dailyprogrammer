module DailyProgrammer.MetroTile where

import Data.Monoid (mappend)
import Data.List (foldl')
import qualified Data.Set as S

main = do
    bounds <- getLine
    chars <- getContents
    let [xb, yb] = map read $ words bounds
        pixels   = toPixels xb yb . concat . lines $ chars
    mapM_ print . S.toAscList . foldl' addPixel S.empty $ pixels

data Window = Window { title  :: Char
                     , xpos   :: Int
                     , ypos   :: Int
                     , width  :: Int
                     , height :: Int
                     }

{- |Equal windows have the same title and position. We don't want to just derive
    Eq here because we will depend on Set to replace an existing window with a
    bigger one. -}
instance Eq Window where
    v == w = (title v == title w) && (xpos v == xpos w) && (ypos v == ypos w)

instance Ord Window where
-- |Compare windows by x position, y position, and title in that order.
    v `compare` w = (xpos v `compare` xpos w) `mappend`
                    (ypos v `compare` ypos w) `mappend`
                    (title v `compare` title w)

instance Show Window where
    show (Window t x y w h) = show w ++ 'x' : show h ++ " tile of character '"
                              ++ t : "' located at (" ++ show x ++ ','
                              : show y ++ ")"

{- |Take an x-bound, y-bound, and a list of characters and generate a list of
    "pixels" represented by 1x1 windows. -}
toPixels :: Int -> Int -> [Char] -> [Window]
toPixels xb yb chars = map mkPix . filter ((/='.') . char) $ zip3 chars xs ys
    where xs = cycle [0..xb-1]
          ys = concat [take xb $ repeat y | y <- [0..yb-1]]
          char (c,_,_) = c
          mkPix (c,x,y) = Window {title=c, xpos=x, ypos=y, width=1, height=1}

{- |Accumulation function for the Set of windows. Take a Set of Windows
    and a pixel. Add the pixel to the Set by either expanding one of the
    windows or adding a new window. Set is implemented as a balanced binary
    search tree, so that should keep the windows ordered and keep insertion
    efficient. O(size wins) -}
addPixel :: S.Set Window -> Window -> S.Set Window
wins `addPixel` p | (not . S.null) contWins = wins
                  | (not . S.null) adjWins  = bigWin `S.insert` wins
                  | otherwise               = p `S.insert` wins
                  where contWins = S.filter (`contains` p) wins
                        adjWins  = S.filter (`adjoins` p) wins
                        bigWin   = (head $ S.toList adjWins) `merge` p

Window t1 x1 y1 w1 h1 `contains` Window t2 x2 y2 _ _
    | t1 /= t2 = False
    | x2 < x1  = False
    | y2 < y1  = False
    | (x2 < x1 + w1) && (y2 < y1 + h1) = True
    | (y2 < y1 + h1) && (x2 < x1 + w1) = True
    | otherwise = False

Window t1 x1 y1 w1 h1 `adjoins` Window t2 x2 y2 _ _
    | t1 /= t2 = False
    | x2 < x1  = False
    | y2 < y1  = False
    | (x2 < x1 + w1) && (y2 == y1 + h1) = True
    | (y2 < y1 + h1) && (x2 == x1 + w1) = True
    | otherwise = False

Window t1 x1 y1 w1 h1 `merge` Window _ x2 y2 _ _
    = Window t1 x1 y1 (max w1 (x2-x1+1)) (max h1 (y2-y1+1))

