module DailyProgrammer.ContiguousChains where
-- https://www.reddit.com/r/dailyprogrammer/comments/3gpjn3/20150812_challenge_227_intermediate_contiguous/

import Data.Matrix (Matrix, (!), fromLists, ncols, nrows, safeGet)
import Data.Graph (Graph, components, graphFromEdges)
import Data.Maybe (isJust, fromJust)

node :: Matrix Char -> (Int, Int) -> Maybe (Char, (Int, Int), [(Int, Int)])
-- Take a matrix of our input data and the indices of an input element.
-- If this element is not an 'x', return Nothing. Otherwise, Return the vertex
-- at the given indices and all connected 'x's as a tuple.
-- Only consider elements to the right and beneath the given vertex. This will
-- result in a digraph where all the edges point right or down.
node m (i,j)
    | m ! (i,j) /= 'x' = Nothing
    | otherwise = Just ('x', (i,j), xCoords adjs)
    where adjs = [(safeGet (i+1) j m, (i+1,j)), (safeGet i (j+1) m, (i,j+1))]
          xCoords = map snd . filter ((== Just 'x') . fst)

nodes :: Matrix Char -> [(Char, (Int, Int), [(Int, Int)])]
nodes m = map fromJust . filter isJust . map (node m) $
                            [(i,j) | i <- [1..(nrows m)], j <- [1..(ncols m)]]

fst3 (x,_,_) = x

graphFromInput :: String -> Graph
graphFromInput = fst3 . graphFromEdges . nodes . fromLists . lines

main = getLine >> getContents >>= print . length . components . graphFromInput

