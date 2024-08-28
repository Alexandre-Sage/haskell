module Week1 (toDigits, toDigitsRev, doubleEveryOther, sumDigits) where

import qualified Data.List as List
import Prelude

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (List.map (read . (: [])) . show) n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = List.reverse (toDigits n)

double :: Integer -> Integer -> Integer
double idx val
  | even idx = val * 2
  | otherwise = val

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = List.reverse . zipWith double [1 ..] . List.reverse

toSimpleDigit :: Integer -> [Integer]
toSimpleDigit n | n > 9 = toDigits n | otherwise = [n]

sumDigits :: [Integer] -> Integer
sumDigits = sum . List.concatMap toSimpleDigit
