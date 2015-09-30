module Kata.WonderlandNumberSpec (main, spec) where

import Test.Hspec

import Kata.WonderlandNumber (wonderlandNumber, toDigitSet)

import Data.Foldable (for_)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "WonderlandNumber" $
    [2..6] `for_` \n ->
      it ("has the same digits when multiplied by " ++ show n) $
        (wonderlandNumber * n) `hasAllTheSameDigitsAs` wonderlandNumber

hasAllTheSameDigitsAs :: Int -> Int -> Expectation
hasAllTheSameDigitsAs n m = toDigitSet n `shouldBe` toDigitSet m
