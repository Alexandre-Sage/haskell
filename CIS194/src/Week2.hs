{-# LANGUAGE FlexibleInstances #-}

module Week2 (LogMessage (..), MessageType (..), parseMessage) where

import Data.Char (digitToInt, isDigit)
import qualified Data.Maybe as Maybe
import GHC.Arr (Array (Array))
import Helpers
import Text.Read (Lexeme (String))

{- PREDEFINED DATA -}

data MessageType = Info | Warning | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String | Unknown String
  deriving (Show, Eq)

{- END PREDEFINED DATA -}

unknowLog :: LogMessage
unknowLog = Unknown "This is not in the right format"

createLogMessage :: MessageType -> String -> String -> LogMessage
createLogMessage x y = LogMessage x $ read y

createMessage :: [String] -> LogMessage
createMessage (x : y : z : strTail)
  | not $ isValidNumber y = unknowLog
  | x == "W" = createLogMessage Warning y $ unwords $ z : strTail
  | x == "E" && isValidNumber z = createLogMessage (Error $ read y) z $ unwords strTail
  | x == "I" = createLogMessage Info y $ unwords $ z : strTail
  | otherwise = unknowLog
createMessage _ = unknowLog

parseMessage :: String -> LogMessage
parseMessage = createMessage . words
