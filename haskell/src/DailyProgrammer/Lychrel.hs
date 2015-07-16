module DailyProgrammer.Lychrel where
{- |
    http://www.reddit.com/r/dailyprogrammer/comments/38yy9s/20150608_challenge_218_easy_making_numbers/
    
    To covert nearly any number into a palindromic number you operate by
    reversing the digits and adding and then repeating the steps until you get a
    palindromic number. Some require many steps.

    e.g. 24 gets palindromic after 1 steps: 66 -> 24 + 42 = 66
-}
import Data.List (span, iterate)

lychrel n = n + (read . reverse . show $ n)
palindrome n = (show n) == (reverse . show $ n)

steps n = (n, length nps, head ps)
    where (nps, ps) = span (not . palindrome) . iterate lychrel $ n

printsteps (n, steps, result) = putStrLn $ (show n) ++ " gets palindromic after "
                                ++ (show steps) ++ " step(s): " ++ (show result)

main = do
        numbers <- getContents
        mapM_ (printsteps . steps) $ map read . lines $ numbers

