module DailyProgrammer.TestISBN where

import Test.HUnit
import DailyProgrammer.ISBN

testCharValue1 = 10 @=? charValue 'X'
testCharValue2 = 0  @=? charValue '-'
testCharValue3 = 3  @=? charValue '3'

testFormat1 = True  @=? isValidFormat "0-7475-3269-9"
testFormat2 = False @=? isValidFormat "0-7475-3269-x"
testFormat3 = True  @=? isValidFormat "0-7475-3269-X"
testFormat4 = False @=? isValidFormat "0-74753-269-X"
testFormat5 = False @=? isValidFormat "0-74X5-3269-9"

testChecksum = 242 @=? checksum "0-7475-3269-9"

testIsValid1 = True  @=? isValid "0-7475-3269-9"
testIsValid2 = False @=? isValid "0-7475-3269-X"
testIsValid3 = False @=? isValid "0-74753-269-9"

