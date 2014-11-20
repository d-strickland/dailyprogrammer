module DailyProgrammer.Test118 where

import Test.HUnit
import Data.Time
import Data.Time.Clock
import Data.Time.Format
import System.Locale

import DailyProgrammer.Challenge118

testTime = readTime defaultTimeLocale "%Y-%m-%d" "1961-11-22" :: UTCTime

testRead1 = testTime @=? getDate "1961-11-22"
testRead2 = testTime @=? getDate "11/22/61"
testRead3 = testTime @=? getDate "11#61#22"
testRead4 = testTime @=? getDate "22*11*1961"
testRead5 = testTime @=? getDate "Nov 22, 61"
testRead6 = testTime @=? getDate "Nov 22, 1961"

testWrite = "1961-11-22" @=? showDate testTime

