module DailyProgrammer.RailFence where

import Data.List (sortBy)
import Data.Ord (comparing)

main = do
    putStrLn $ enc 3 "REDDITCOMRDAILYPROGRAMMER"
    putStrLn $ dec 3 "RIMIRAREDTORALPORMEDCDYGM"

zigZag n = cycle $ [1..n] ++ [n-1..2]
xs `sortedBy` ys = map fst (sortBy (comparing snd) $ zip xs ys)
enc n message = message `sortedBy` zigZag n
dec n digest = digest `sortedBy` enc n [1..(length digest)]

