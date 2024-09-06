module Week1Tests (testsWeek1) where

import Helpers (assertMessage)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual, failures)
import Week1 (doubleEveryOther, hanoi, sumDigits, toDigits, toDigitsRev)

toDigittest :: Integer -> [Integer] -> Test
toDigittest testValue expectedValue = TestCase $ assertEqual (assertMessage testValue expectedValue) (toDigits testValue) expectedValue

toDigitsRevTest :: Integer -> [Integer] -> Test
toDigitsRevTest testValue expectedValue = TestCase $ assertEqual (assertMessage testValue expectedValue) (toDigitsRev testValue) expectedValue

doubleEveryotherTest :: Integer -> [Integer] -> Test
doubleEveryotherTest testValue expectedValue = TestCase $ assertEqual (assertMessage testValue expectedValue) (doubleEveryOther $ toDigits testValue) expectedValue

sumDigitsTest :: [Integer] -> Integer -> Test
sumDigitsTest testValue expectedValue = TestCase $ assertEqual (assertMessage expectedValue testValue) (sumDigits testValue) expectedValue

hanoiTest :: Integer -> String -> String -> String -> [(String, String)] -> Test
hanoiTest disk peg1 peg2 peg3 expected = TestCase $ assertEqual ("Should return" ++ show expected ++ "for" ++ show disk ++ "disk and" ++ peg1 ++ peg2 ++ peg3 ++ "pegs") (hanoi disk peg1 peg2 peg3) expected

testsWeek1 :: Test
testsWeek1 =
  TestList
    [ TestLabel "toDigits" $ toDigittest 1234 [1, 2, 3, 4],
      toDigittest 0 [],
      toDigittest (-17) [],
      TestLabel
        "toDigitsRev"
        $ toDigitsRevTest 1234 [4, 3, 2, 1],
      TestLabel
        "doubleEveryOther"
        $ doubleEveryotherTest 8765 [16, 7, 12, 5],
      doubleEveryotherTest 123 [1, 4, 3],
      doubleEveryotherTest 4683 [8, 6, 16, 3],
      TestLabel
        "sumDigits"
        $ sumDigitsTest [16, 7, 12, 5] 22,
      TestLabel "hanoi" $ hanoiTest 2 "a" "b" "c" [("a", "c"), ("a", "b"), ("c", "b")]
    ]
