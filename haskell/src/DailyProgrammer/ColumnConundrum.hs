module DailyProgrammer.ColumnConundrum where
{--|
Input Description
=================
To start, you will be given 3 numbers on one line:
<number of columns> <column width> <space width>

number of columns: The number of columns to collect the text into.
column width: The width, in characters, of each column.
space width: The width, in spaces, of the space between each column.

After that first line, the rest of the input will be the text to format.

Output Description
==================
You will print the text formatted into the appropriate style.
You do not need to account for words and spaces. If you wish, cut a word into
two, so as to keep the column width constant.

Extension
=========
Split words correctly, like in my sample output.
--}
import Data.List
import Data.List.Split (chunksOf)

main = do
    args <- getLine
    text <- getContents
    let [n, w, s] = map read . words $ args
    mapM_ putStrLn . rows n w s $ text

rows n w s = map (intercalate (take s . repeat $ ' ')) . easyWrap n w

easyWrap :: Int -> Int -> String -> [[String]]
easyWrap n w text = transpose . chunksOf height $ chunks
    where chunks = chunksOf w . concat . lines $ text
          height = (length chunks) `cdiv` n

x `cdiv` y = 1 + ((x - 1) `div` y) -- Divide two Ints and round towards infinity

