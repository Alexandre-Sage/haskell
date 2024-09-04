module Main (main) where

import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)
import Week1Tests (testsWeek1)
import Week2Tests (testsWeek2)

main :: IO ()
main = do
  result <- runTestTT $ TestList [testsWeek1, testsWeek2]
  print (show result)
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
