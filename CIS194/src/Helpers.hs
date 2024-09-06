{-# LANGUAGE FlexibleInstances #-}

module Helpers (IsValidNumber (..)) where

import Data.Char (isDigit)

class IsValidNumber a where
  isValidNumber :: a -> Bool

instance IsValidNumber Char where
  isValidNumber = isDigit

instance IsValidNumber String where
  isValidNumber = Prelude.all isDigit
