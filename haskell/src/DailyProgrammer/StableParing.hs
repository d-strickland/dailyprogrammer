module DailyProgrammer.StablePairing where
-- https://www.reddit.com/r/dailyprogrammer/comments/3kj1v9/20150911_challenge_231_hard_eight_husbands_for/
import qualified Data.List.Zipper as Z
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (elemIndex)

data PairData = PairData { prefs1       :: M.Map Char (Z.Zipper Char)
                         , prefs2       :: M.Map Char [Char]
                         , available    :: [Char]
                         , satisfaction :: M.Map Char Int
                         , pairs        :: M.Map Char Char
                         }

stablePairing :: State PairData (M.Map Char Char)
stablePairing = do
    pd <- get
    if null $ available pd
    then return $ pairs pd
    else do
        propose
        stablePairing

propose :: State PairData ()
propose = do
    pd <- get
    let proposer = head $ available pd
        proposee = Z.cursor $ (prefs1 pd) M.! proposer
        preference = fromJust . elemIndex proposer $ (prefs2 pd) M.! proposer
    if preference >= (satisfaction pd) then return ()
    else return () --modify $ \s -> s { prefs1 = 


main = putStrLn "Types check out."

