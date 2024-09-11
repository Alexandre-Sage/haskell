{-# LANGUAGE FlexibleInstances #-}

module Week2 (LogMessage (..), MessageType (..), parseMessage, parse, MessageTree (..), insert, build, inorder, whatWentWrong) where

import qualified Data.List as List
import Helpers

{- PREDEFINED DATA -}

data MessageType = Info | Warning | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String | Unknown String
  deriving (Show, Eq)

data MessageTree
  = Leaf
  | Node MessageTree LogMessage MessageTree
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

parse :: String -> [LogMessage]
parse = List.map parseMessage . lines

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (Unknown _) = -1
getTimeStamp (LogMessage _ x _) = x

isUnknow :: LogMessage -> Bool
isUnknow (LogMessage {}) = False
isUnknow (Unknown _) = True

insert :: LogMessage -> MessageTree -> MessageTree
insert newLog Leaf = Node Leaf newLog Leaf
insert newLog (Node left cur right)
  | isUnknow newLog = Node left cur right
  | getTimeStamp newLog < getTimeStamp cur = Node (insert newLog left) cur right
  | otherwise = Node left cur $ insert newLog right

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node left val right) = inorder left <> (val : inorder right)

extractMessage :: LogMessage -> String
extractMessage (Unknown msg) = msg
extractMessage (LogMessage _ _ msg) = msg

isOver50Error :: LogMessage -> Bool
isOver50Error (LogMessage (Error x) _ _)
  | x >= 50 = True
  | otherwise = False
isOver50Error _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = List.map extractMessage . List.filter isOver50Error
