module DailyProgrammer.EstimatePI where
-- https://www.reddit.com/r/dailyprogrammer/comments/3f0hzk/20150729_challenge_225_intermediate_estimating_pi/

import Data.Either
import Data.List.Split (chunksOf)
import System.Environment (getArgs)
import Vision.Image
import Vision.Image.Storage.DevIL
import Vision.Primitive.Shape (Z (..), (:.) (..))
import qualified Data.Vector.Storable as V

black = RGBPixel 0 0 0

height ((Z :. h) :. _) = h

area :: RGB -> Int
area = V.length . V.filter (==black) . manifestVector

diameter :: RGB -> Int
diameter img = length . filter (black `elem`) . chunksOf h . V.toList
                        . manifestVector $ img
    where h = height . manifestSize $ img

piEst :: RGB -> Double
piEst circle = a / (r*r)
    where a = (fromIntegral . area $ circle)
          r = (fromIntegral . diameter $ circle) / 2.0

main :: IO ()
main = getArgs >>= return . head >>= load Autodetect
        >>= either (fail "Could not load image.") (print . piEst)
    
