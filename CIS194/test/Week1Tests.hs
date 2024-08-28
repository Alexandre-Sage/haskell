module Week1Tests (tests) where

import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual, failures)
import Week1 (doubleEveryOther, sumDigits, toDigits, toDigitsRev)

assertMessage :: Integer -> [Integer] -> String
assertMessage testValue expectedValue = "Should return " ++ show expectedValue ++ " for " ++ show testValue

toDigittest :: Integer -> [Integer] -> Test
toDigittest testValue expectedValue = TestCase (assertEqual (assertMessage testValue expectedValue) (toDigits testValue) expectedValue)

toDigitsRevTest :: Integer -> [Integer] -> Test
toDigitsRevTest testValue expectedValue = TestCase (assertEqual (assertMessage testValue expectedValue) (toDigitsRev testValue) expectedValue)

doubleEveryotherTest :: Integer -> [Integer] -> Test
doubleEveryotherTest testValue expectedValue = TestCase (assertEqual (assertMessage testValue expectedValue) (doubleEveryOther (toDigits testValue)) expectedValue)

sumDigitsTest :: [Integer] -> Integer -> Test
sumDigitsTest testValue expectedValue = TestCase (assertEqual ("Should teturn " ++ show expectedValue ++ " for " ++ show testValue) (sumDigits testValue) expectedValue)

tests :: Test
tests =
  TestList
    [ TestLabel "toDigits" (toDigittest 1234 [1, 2, 3, 4]),
      toDigittest 0 [],
      toDigittest (-17) [],
      TestLabel
        "toDigitsRev"
        (toDigitsRevTest 1234 [4, 3, 2, 1]),
      TestLabel
        "doubleEveryOther"
        (doubleEveryotherTest 8765 [16, 7, 12, 5]),
      doubleEveryotherTest 123 [1, 4, 3],
      doubleEveryotherTest 4683 [8, 6, 16, 3],
      TestLabel "sumDigits" (sumDigitsTest [16, 7, 12, 5] 22)
    ]
