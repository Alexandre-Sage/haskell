module Week2Tests (testsWeek2) where

import qualified Data.List as List
import Helpers (assertMessage)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual, failures)
import Week2 (LogMessage (..), MessageType (Error, Info, Warning), parseMessage)

logs =
  [ ("I 6 Completed armadillo processing", LogMessage Info 6 "Completed armadillo processing"),
    ("I 1 Nothing to report", LogMessage Info 1 "Nothing to report"),
    ("I 4 Everything normal", LogMessage Info 4 "Everything normal"),
    ("I 11 Initiating self-destruct sequence", LogMessage Info 11 "Initiating self-destruct sequence"),
    ("E 70 3 Way too many pickles", LogMessage (Error 70) 3 "Way too many pickles"),
    ("E 65 8 Bad pickle-flange interaction detected", LogMessage (Error 65) 8 "Bad pickle-flange interaction detected"),
    ("W 5 Flange is due for a check-up", LogMessage Warning 5 "Flange is due for a check-up"),
    ("I 7 Out for lunch, back in two time steps", LogMessage Info 7 "Out for lunch, back in two time steps"),
    ("E 20 2 Too many pickles", LogMessage (Error 20) 2 "Too many pickles"),
    ("I 9 Back from lunch", LogMessage Info 9 "Back from lunch"),
    ("E 99 10 Flange failed!", LogMessage (Error 99) 10 "Flange failed!"),
    ("E 2 562 help help", LogMessage (Week2.Error 2) 562 "help help"),
    ("I 29 la la la", LogMessage Info 29 "la la la")
  ]

parseMessageTest :: (String, LogMessage) -> Test
parseMessageTest (testValue, expectedValue) =
  TestCase $
    assertEqual (assertMessage testValue expectedValue) (parseMessage testValue) expectedValue

tests :: [(String, LogMessage)] -> Test
tests arr = TestList $ List.map parseMessageTest arr

testsWeek2 :: Test
testsWeek2 = TestList [TestLabel "Parse message" $ tests logs]
