module Kata.WonderlandNumber
       (
         wonderlandNumber
       , toDigitSet
       ) where

import Control.Arrow ((>>>))
import Control.Monad (guard)
import qualified Data.IntSet as IntSet
import qualified Data.Digits as Digits

-- | Return one wonderland number. We do not care if there is more than
-- one.
wonderlandNumber :: Int
wonderlandNumber =
  case wonderlandNumbers of
    (w:_) -> w
    [] -> error "there is no wonderland number"

-- | Brute force search! At least it's lazy.
wonderlandNumbers :: [Int]
wonderlandNumbers = do
  d1 <- [1..9]
  d2 <- [0..9]
  d3 <- [0..9]
  d4 <- [0..9]
  d5 <- [0..9]
  d6 <- [0..9]
  let ds = [d1, d2, d3, d4, d5, d6]
  let wonderSet = IntSet.fromList ds
  let wonder = Digits.unDigits 10 ds
  guard $ all (== wonderSet) $ do
    n <- [2..6]
    return (toDigitSet (wonder * n))
  return wonder

-- | Return a set of base-10 digits for a non-negative integer.
toDigitSet :: Int -> IntSet.IntSet
toDigitSet = Digits.digitsRev 10 >>> IntSet.fromList
