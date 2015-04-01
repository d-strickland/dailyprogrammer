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

rows n w s = map (intercalate (take s . repeat $ ' ')) . wrap n . toughChunks w

wrap n chunks = transpose . chunksOf height $ chunks
    where height = (length chunks) `cdiv` n
          x `cdiv` y = 1 + ((x - 1) `div` y) -- Ceiling division

easyChunks :: Int -> String -> [String]
easyChunks w = chunksOf w . concat . lines

toughChunks :: Int -> String -> [String]
toughChunks width = accum [] . intercalate ["\n"] . map words . lines
    where accum [] [] = []
          accum cs [] = cs
          accum [] (w:ws) = accum [w] ws
          accum (c:cs) (w:ws)
            | w == "\n" = (c ++ (take rmdr . repeat $ ' ')):(accum cs ws)
            | rmdr - (length w) - 1 < 0
                        = (c ++ (take rmdr . repeat $ ' ')):(accum cs (w:ws))
            | otherwise = accum ((c ++ " " ++ w):cs) ws
                where rmdr = width - (length c)

