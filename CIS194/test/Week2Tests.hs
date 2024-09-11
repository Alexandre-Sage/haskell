{-# LANGUAGE BlockArguments #-}

module Week2Tests (testsWeek2, testIoWeek2) where

import qualified Data.List as List
import Helpers (assertMessage)
import Test.HUnit (Test (TestCase, TestLabel, TestList), Testable (test), assertEqual, failures)
import Week2 (LogMessage (..), MessageTree (Leaf, Node), MessageType (Error, Info, Warning), build, inorder, parse, parseMessage, whatWentWrong)

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

expectLogStrFormated =
  [ LogMessage Info 6 "Completed armadillo processing",
    LogMessage Info 1 "Nothing to report",
    LogMessage Info 4 "Everything normal",
    LogMessage Info 11 "Initiating self-destruct sequence",
    LogMessage (Error 70) 3 "Way too many pickles",
    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
    LogMessage Warning 5 "Flange is due for a check-up",
    LogMessage Info 7 "Out for lunch, back in two time steps",
    LogMessage (Error 20) 2 "Too many pickles",
    LogMessage Info 9 "Back from lunch",
    LogMessage (Error 99) 10 "Flange failed!"
  ]

expectedSortedLog =
  [ LogMessage Info 1 "Nothing to report",
    LogMessage (Error 20) 2 "Too many pickles",
    LogMessage (Error 70) 3 "Way too many pickles",
    LogMessage Info 4 "Everything normal",
    LogMessage Warning 5 "Flange is due for a check-up",
    LogMessage Info 6 "Completed armadillo processing",
    LogMessage Info 7 "Out for lunch, back in two time steps",
    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
    LogMessage Info 9 "Back from lunch",
    LogMessage (Error 99) 10 "Flange failed!",
    LogMessage Info 11 "Initiating self-destruct sequence"
  ]

expectedTree =
  Node
    ( Node (Node (Node Leaf (LogMessage Info 1 "Nothing to report") Leaf) (LogMessage (Error 20) 2 "Too many pickles") (Node (Node (Node Leaf (LogMessage (Error 70) 3 "Way too many pickles") (Node Leaf (LogMessage Info 4 "Everything normal") Leaf)) (LogMessage Warning 5 "Flange is due for a check-up") (Node Leaf (LogMessage Info 6 "Completed armadillo processing") Leaf)) (LogMessage Info 7 "Out for lunch, back in two time steps") (Node Leaf (LogMessage (Error 65) 8 "Bad pickle-flange interaction detected") Leaf))) (LogMessage Info 9 "Back from lunch") Leaf
    )
    (LogMessage (Error 99) 10 "Flange failed!")
    (Node Leaf (LogMessage Info 11 "Initiating self-destruct sequence") Leaf)

expectedFinalMessages =
  [ "Way too many pickles",
    "Bad pickle-flange interaction detected",
    "Flange failed!"
  ]

parseMessageTest :: (String, LogMessage) -> Test
parseMessageTest (testValue, expectedValue) =
  TestCase $
    assertEqual (assertMessage testValue expectedValue) (parseMessage testValue) expectedValue

parseMessageSuite :: [(String, LogMessage)] -> Test
parseMessageSuite arr = TestList $ List.map parseMessageTest arr

parseTest :: FilePath -> IO Test
parseTest file = do
  content <- readFile file
  return $ TestCase $ assertEqual "Should parse the log content" (parse content) expectLogStrFormated

buildTest :: FilePath -> IO Test
buildTest file = do
  content <- readFile file
  return $ TestCase $ assertEqual "Should parse log content as binary tree" ((build . parse) content) expectedTree

inOrderTest :: FilePath -> IO Test
inOrderTest file = do
  content <- readFile file
  return $ TestCase $ assertEqual "Should sort" ((inorder . build . parse) content) expectedSortedLog

whatWentWrongTest :: FilePath -> IO Test
whatWentWrongTest file = do
  content <- readFile file
  return $ TestCase $ assertEqual "Should show what went wrong" ((whatWentWrong . inorder . build . parse) content) expectedFinalMessages

testsWeek2 :: Test
testsWeek2 = TestList [TestLabel "Parse message" $ parseMessageSuite logs]

testIoWeek2 :: IO Test
testIoWeek2 =
  do
    test <- parseTest "test/input/log.txt"
    test2 <- buildTest "test/input/log.txt"
    test3 <- inOrderTest "test/input/log.txt"
    test4 <- whatWentWrongTest "test/input/log.txt"
    return $ TestList [TestLabel "parse" test, TestLabel "build" test2, TestLabel "inorder" test3, TestLabel "what went wrong" test4]
