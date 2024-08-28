module Main where

import qualified MyLib (someFunc)
import Week1 (toDigits)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn (show (toDigits 1234))
  MyLib.someFunc
