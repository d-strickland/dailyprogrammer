module DailyProgrammer.BankNumbers where
import Data.List (transpose, foldl', elemIndex)
import Data.Char (digitToInt, intToDigit)

main = do putStrLn . banner $ "0123456789"
          putStrLn . showFromBanner . fromBanner . banner $ "0123456789"

banner :: String -> String
banner = unlines . transpose . bannerT

bannerT :: String -> [String]
bannerT = foldl' acc []
    where acc xs x = xs ++ (transpose $ nums!!(digitToInt x))

fromBanner :: String -> Maybe [Int]
fromBanner = sequence . fromBannerT . transpose . lines

fromBannerT :: [String] -> [Maybe Int]
fromBannerT [] = []
fromBannerT (x:y:z:xs) = (elemIndex (transpose [x,y,z]) nums) : (fromBannerT xs)

showFromBanner Nothing = "Bad banner."
showFromBanner (Just ns) = map intToDigit ns

nums = [[" _ ",
         "| |",
         "|_|"],

        ["   ",
         "  |",
         "  |"],

        [" _ ",
         " _|",
         "|_ "],

        [" _ ",
         " _|",
         " _|"],

        ["   ",
         "|_|",
         "  |"],

        [" _ ",
         "|_ ",
         " _|"],

        [" _ ",
         "|_ ",
         "|_|"],

        [" _ ",
         "  |",
         "  |"],

        [" _ ",
         "|_|",
         "|_|"],

        [" _ ",
         "|_|",
         " _|"]]

