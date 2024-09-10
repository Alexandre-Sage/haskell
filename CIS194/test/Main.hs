module Main (main) where

import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)
import Week1Tests (testsWeek1)
import Week2 (LogMessage (..), MessageTree (..), MessageType (..), insert)
import Week2Tests (testIoWeek2, testsWeek2)

main :: IO ()
main = do
  result <- runTestTT $ TestList [testsWeek1, testsWeek2]
  ioTest <- testIoWeek2
  ioRes <- runTestTT ioTest
  print (show result)
  if failures result > 0 || failures ioRes > 0 then Exit.exitFailure else Exit.exitSuccess
