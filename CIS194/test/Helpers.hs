module Helpers (assertMessage) where

import GHC.Generics

assertMessage :: (Show testValue) => (Show expectedValue) => testValue -> expectedValue -> String
assertMessage testValue expectedValue = "Should return " ++ show expectedValue ++ " for " ++ show testValue
