module Main (main) where

import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), runTestTT)
import Week1Tests (tests)

main :: IO ()
main = do
  result <- runTestTT tests
  print (show result)
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
