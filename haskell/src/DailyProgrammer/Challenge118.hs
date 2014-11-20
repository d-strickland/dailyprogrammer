module DailyProgrammer.Challenge118 where
-- iso 8601 standard for dates tells us the proper way to do an extended day is
-- yyyy-mm-dd
-- 
--     yyyy = year
--     mm = month
--     dd = day
-- 
-- A company's database has become polluted with mixed date formats. They could
-- be one of 6 different formats
-- 
--     yyyy-mm-dd
--     mm/dd/yy
--     mm#yy#dd
--     dd*mm*yyyy
--     (month word) dd, yy
--     (month word) dd, yyyy
-- 
-- (month word) can be: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
-- 
-- Note if is yyyy it is a full 4 digit year. If it is yy then it is only the
-- last 2 digits of the year. Years only go between 1950-2049.

import Data.Time
import Data.Time.Format
import System.Locale
import Text.Regex.Posix

main118 = do
        line <- getLine
        if null line
            then return ()
            else do
                putStrLn $ (showDate . getDate) line
                main118


getDate :: [Char] -> UTCTime
getDate timeStr  | rawTime < maxTime = rawTime
                 | otherwise         = previousCentury rawTime
    where rawTime = rdTime format timeStr
          format | timeStr =~ "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"       = "%Y-%m-%d"
                 | timeStr =~ "^[0-9]{2}/[0-9]{2}/[0-9]{2}$"       = "%m/%d/%y"
                 | timeStr =~ "^[0-9]{2}#[0-9]{2}#[0-9]{2}$"       = "%m#%y#%d"
                 | timeStr =~ "^[0-9]{2}\\*[0-9]{2}\\*[0-9]{4}$"   = "%d*%m*%Y"
                 | timeStr =~ "^[A-Z][a-z]{2} [0-9]{2}, [0-9]{2}$" = "%b %d, %y"
                 | timeStr =~ "^[A-Z][a-z]{2} [0-9]{2}, [0-9]{4}$" = "%b %d, %Y"
                 | otherwise = error "Bad date format."

showDate :: UTCTime -> [Char]
showDate = fmtTime "%Y-%m-%d"

previousCentury :: UTCTime -> UTCTime
previousCentury time = rdTime "%Y-%m-%d" ("19" ++ timeStr)
                       where timeStr = fmtTime "%y-%m-%d" time

maxTime = readTime defaultTimeLocale "%Y-%m-%d" "2050-01-01"

fmtTime = formatTime defaultTimeLocale
rdTime = readTime defaultTimeLocale
