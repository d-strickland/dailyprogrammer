module DailyProgrammer.Fraction where

gcd' :: Integral a => a -> a -> a
gcd' x 0 = x
gcd' x y = gcd' y (x `rem` y)

fracMain = do
    print $ gcd 4 6

