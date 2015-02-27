module DailyProgrammer.BankNumbers where

import qualified Data.Map as M
import Data.List (foldl')

data Window = Window { title :: Char
                     , xpos :: Int
                     , ypos :: Int
                     , width :: Int
                     , height :: Int
                     } deriving (Show)

windows :: Int -> [Window] -> String -> [Window]
windows _ ws [] = ws -- Base case: Return our windows if the string is empty
windows y ws '.':xs = windows y ws xs
windows y [] x:xs = windows y (Window x BLARG
windows y c x:xs | c == x = 

