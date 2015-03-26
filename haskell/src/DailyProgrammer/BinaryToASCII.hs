module DailyProgrammer.BinaryToASCII where
{--|
Input description
=================
On console input you will be given a variable number of 0's and 1's that
correspond to letters in the alphabet [a-z] and whitespace ' '. These will be
integers coming in, it's your job to cast them however you need.

Output description
==================
The program should output the english translation (or other languages if you
feel so inclined!) of the binary phrase
--}

import Data.Char (chr, digitToInt)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)

toBase10 = sum . zipWith (*) powers . map digitToInt
         where powers = map (2^) [7,6..0]

toASCII = map (chr . toBase10) . chunksOf 8

main = getArgs >>= (putStrLn . toASCII . concat)

