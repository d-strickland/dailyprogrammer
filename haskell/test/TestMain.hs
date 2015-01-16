module Main (
    main
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import DailyProgrammer.Test118
import DailyProgrammer.TestISBN

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Daily Programmer Problems"
    [
        testGroup "ISO 8601"
        [
            testCase "Read 1" testRead1,
            testCase "Read 2" testRead2,
            testCase "Read 3" testRead3,
            testCase "Read 4" testRead4,
            testCase "Read 5" testRead5,
            testCase "Read 6" testRead6,

            testCase "Write" testWrite
        ],

        testGroup "ISBN"
        [
            testCase "CharValue 1" testCharValue1,
            testCase "CharValue 2" testCharValue2,
            testCase "CharValue 3" testCharValue3,
            
            testCase "Format 1" testFormat1,
            testCase "Format 2" testFormat2,
            testCase "Format 3" testFormat3,
            testCase "Format 4" testFormat4,
            testCase "Format 5" testFormat5,

            testCase "Checksum" testChecksum,

            testCase "Validity 1" testIsValid1,
            testCase "Validity 2" testIsValid2,
            testCase "Validity 3" testIsValid3
        ]
    ]

