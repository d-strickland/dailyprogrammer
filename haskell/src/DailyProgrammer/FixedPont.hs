module DailyProgrammer.FixedPoint where
-- https://www.reddit.com/r/dailyprogrammer/comments/3i99w8/20150824_challenge_229_easy_the_dottie_number/
import Data.List (iterate)

fixedPoint :: Float -> Float -> (Float -> Float) -> Float
fixedPoint start err f = fst . head . reverse . takeWhile ((> err) . snd) . iterate next $ (start, err + 1)
    where next (n, e) = (f n, abs $ n - (f n))

main = do
    let err = 1e-6
    print . fixedPoint 1 err $ cos
    print . fixedPoint 2 err $ (\x -> x - (tan x))
    print . fixedPoint 1 err $ (\x -> 1 + (1/x))
    print . fixedPoint 0.5 err $ (\x -> 4 * x * (1-x))
