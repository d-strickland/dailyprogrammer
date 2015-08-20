module DailyProgrammer.Fraction where
-- https://www.reddit.com/r/dailyprogrammer/comments/3fmke1/20150803_challenge_226_easy_adding_fractions/
import Data.List (foldl')
import Data.List.Split (splitOn)

gcd' :: Integer -> Integer -> Integer
gcd' x y = abs $ gcdHelper x y

gcdHelper x 0 = x
gcdHelper x y = gcdHelper y (x `rem` y)

data Fraction = Fraction Integer Integer deriving (Eq)

-- Return a fraction where the numerator and denominator are relatively prime,
-- and the denominator is positive.
fraction :: Integer -> Integer -> Fraction
fraction _ 0 = error "Denominator must be non-zero"
fraction n d = Fraction (n `div` cf) (d `div` cf)
    where cf = (gcd' n d) * (signum d)

instance Show Fraction where
    show (Fraction n 1) = show n
    show (Fraction n d) = show n ++ "/" ++ show d

fromStr :: String -> Fraction
fromStr str = fraction a b
    where [a, b] = map read . splitOn "/" $ str

-- Overloads +, -, *
-- (+), (*), abs, signum, fromInteger, (negate | (-))
instance Num Fraction where
    (Fraction a b) + (Fraction c d) = fraction (a*d + b*c) (b*d)

    (Fraction a b) * (Fraction c d) = fraction (a*c) (b*d)

    abs (Fraction a b) = fraction (abs a) (abs b)

    fromInteger n = Fraction n 1

    signum (Fraction a b) = fromInteger $ (signum a) * (signum b)

    negate (Fraction a b) = fraction (-a) b

main = do
    getLine
    getContents >>= print . foldl' (+) 0 . map fromStr . lines 
