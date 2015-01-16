module DailyProgrammer.ISBN where

import Data.Char (digitToInt, isDigit)
import System.Environment
import Text.Regex.Posix

main197 :: IO()
main197 = do
    args <- getArgs
    if null args
        then putStrLn "Please provide an ISBN."
        else (putStrLn . show . isValid . head) args

isValid isbn = (isValidFormat isbn) && (11 `divides` checksum isbn)
               where divides m n = (n `rem` m) == 0

isValidFormat isbn = isbn =~ "^[0-9]-([0-9]{4}-){2}[0-9X]$"

checksum isbn = sum $ zipWith (*) weights (map charValue isbn)
                where weights = [10,0,9,8,7,6,0,5,4,3,2,0,1]

charValue c | c == '-'  = 0
            | c == 'X'  = 10
            | isDigit c = digitToInt c
            | otherwise = error "ISBN was not formatted correctly."

