module DailyProgrammer.StablePairing where
-- https://www.reddit.com/r/dailyprogrammer/comments/3kj1v9/20150911_challenge_231_hard_eight_husbands_for/
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (elemIndex)

newtype Man = Man Char
    deriving (Eq, Ord, Read, Show)
newtype Woman = Woman Char
    deriving (Eq, Ord, Read, Show)

data PairData = PairData { malePrefs        :: M.Map Man [Woman]
                         , femalePrefs      :: M.Map Woman [Man]
                         , availableMen     :: [Man]
                         , engagementScores :: M.Map Woman Int
                         , pairs            :: M.Map Woman Man
                         }
    deriving (Show)

stablePairing :: State PairData (M.Map Woman Man)
stablePairing = do
    pd <- get
    if null $ availableMen pd then gets pairs
    else propose >> stablePairing


propose :: State PairData ()
propose = do
    man <- popMan
    woman <- popWoman man
    currentScore <- getCurrentScore woman
    newScore <- computeScore woman man
    if newScore <= currentScore then pushMan man
    else getPartner woman >>= maybe (return ()) pushMan >> pushPair woman man newScore


popMan :: State PairData Man
popMan = do
    men <- gets availableMen
    modify $ \s -> s { availableMen = tail $ availableMen s }
    return (head men)

popWoman :: Man -> State PairData Woman
popWoman man = do
    prefs <- gets malePrefs
    let women = prefs M.! man
    modify $ \s -> s { malePrefs = M.insert man (tail women) (malePrefs s) }
    return (head women)

pushMan :: Man -> State PairData ()
pushMan man = modify $ \s -> s { availableMen = man : availableMen s }

getCurrentScore :: Woman -> State PairData Int
getCurrentScore woman = liftM (M.! woman) (gets engagementScores)

computeScore :: Woman -> Man -> State PairData Int
computeScore woman man =
    liftM (fromJust . elemIndex man . (M.! woman)) $ gets femalePrefs

pushPair :: Woman -> Man -> Int -> State PairData ()
pushPair woman man score = modify $ \s ->
                s { pairs = M.insert woman man (pairs s)
                  , engagementScores = M.insert woman score (engagementScores s)
                  }

getPartner :: Woman -> State PairData (Maybe Man)
getPartner woman = liftM (M.lookup woman) $ gets pairs

