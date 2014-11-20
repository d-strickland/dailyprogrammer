module Main (
    main
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import DailyProgrammer.Test118

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
        ]
    ]

