module Week1 (toDigits, toDigitsRev, doubleEveryOther, sumDigits, hanoi) where

import Data.Char (GeneralCategory (MathSymbol))
import qualified Data.List as List
import Prelude

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (List.map (read . (: [])) . show) n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = List.reverse . toDigits

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

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disks src dest tmp | disks == 0 = [] | otherwise = hanoi (disks - 1) src tmp dest ++ [(src, dest)] ++ hanoi (disks - 1) tmp dest src
