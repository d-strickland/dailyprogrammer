module DailyProgrammer.BowlingScore where
-- https://www.reddit.com/r/dailyprogrammer/comments/3ntsni/20151007_challenge_235_intermediate_scoring_a/
import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- getLine
    let shiftLeft fs = (tail fs) ++ [Frame (Roll 0) (Roll 0)]
        frames = map frameFromString . filter (/="") . splitOn " " $ input
        offBy1 = shiftLeft frames
        offBy2 = shiftLeft offBy1
    print . sum $ zipWith3 frameScore frames offBy1 offBy2

data Roll = Roll Int | Spare | Strike
    deriving (Eq, Show)

data Frame = Frame Roll Roll
           | StrikeFrame
           | BonusFrame Roll Roll Roll
    deriving (Eq, Show)

-- Convert a character into a roll
rollFromChar :: Char -> Roll
rollFromChar 'X' = Strike
rollFromChar '/' = Spare
rollFromChar '-' = Roll 0
rollFromChar c = Roll $ read [c]

-- Convert a string into a frame
frameFromString :: String -> Frame
frameFromString "X" = StrikeFrame
frameFromString [r1, r2] = Frame (rollFromChar r1) (rollFromChar r2)
frameFromString [r1, r2, r3] =
    BonusFrame (rollFromChar r1) (rollFromChar r2) (rollFromChar r3)

-- Compute the score of a roll
rollScore :: Roll -> Int
rollScore Strike = 10 
rollScore Spare = undefined  -- Can't say the sore for a spare without prev. roll
rollScore (Roll n) = n

-- Compute the score of a single frame without any strike or spare bonuses
rawFrameScore :: Frame -> Int
rawFrameScore (Frame _ Spare) = 10
rawFrameScore (Frame r1 r2) = rollScore r1 + rollScore r2
rawFrameScore StrikeFrame = 10
rawFrameScore (BonusFrame _ Spare r3) = 10 + rollScore r3
rawFrameScore (BonusFrame r1 _ Spare) = 10 + rollScore r1
rawFrameScore (BonusFrame r1 r2 r3) = sum $ map rollScore [r1, r2, r3]

-- Take three consecutive frames and compute the score of the first frame.
-- Account for strike and spare bonuses.
frameScore :: Frame -> Frame -> Frame -> Int
frameScore StrikeFrame StrikeFrame StrikeFrame = 30
frameScore StrikeFrame StrikeFrame (Frame r1 _) = 20 + rollScore r1
frameScore StrikeFrame f2 _ = 10 + rawFrameScore f2
frameScore (Frame _ Spare) StrikeFrame _ = 20
frameScore (Frame _ Spare) (Frame r1 _) _ = 10 + rollScore r1
frameScore f1 _ _ = rawFrameScore f1

