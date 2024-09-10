module Main where

import Control.Monad.IO.Class
import Week2

main :: IO ()
main = do
  let res = insert (LogMessage Info 11 "test 11") (Node Leaf (LogMessage Info 12 "test 12") Leaf)
  print (show res)
