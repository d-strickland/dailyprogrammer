module DailyProgrammer.StablePairing where
-- https://www.reddit.com/r/dailyprogrammer/comments/3kj1v9/20150911_challenge_231_hard_eight_husbands_for/
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (elemIndex)

data PairData = PairData { malePrefs       :: M.Map Char String
                         , femalePrefs     :: M.Map Char String
                         , availableMen    :: String
                         , engagementScore :: M.Map Char Int
                         , pairs           :: M.Map Char Char
                         }

stablePairing :: State PairData (M.Map Char Char)
stablePairing = do
    pd <- get
    if null $ availableMen pd
    then return $ pairs pd
    else do
        propose
        stablePairing


propose :: State PairData ()
propose = do
    pd <- get
    man <- popMan
    let woman = head $ malePrefs pd M.! man
        newScore = fromJust . elemIndex man $ femalePrefs pd M.! man
    nextWoman man
    when (newScore > engagementScore pd M.! man) $ do
        modifyAvailableMen tail
        setScore woman newScore
        when (member man $ pairs pd) $ modifyAvailableMen
        

nextWoman :: Char -> State PairData ()
nextWoman man = modify $ \s -> s { malePrefs = M.adjust tail man $ malePrefs s }

modifyAvailableMen :: (String -> String) -> State PairData ()
modifyAvailableMen f = modify $ \s -> s { availableMen = f $ availableMen s }

popMan :: State PairData (Char)
popMan = modify $ \s -> s { availableMen = tail availableMen s } >>= return . head $ avaliableMen s

setScore :: Char -> Int -> State PairData ()
setScore woman score = modify $ \s -> s { engagementScore =
                                M.insert woman score $ engagementScore s } 


