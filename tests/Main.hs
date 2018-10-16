{-# language TemplateHaskell #-}
module Main where

import InspectionTests

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Consy
import qualified Data.List

prop_transpose_correct :: Property
prop_transpose_correct =
  property $ do
    str <-
      forAll $
      Gen.list (Range.constant 0 10) $
      Gen.string (Range.constant 0 10) Gen.unicode
    Data.List.transpose str === Consy.transpose str

prop_permutations_correct :: Property
prop_permutations_correct =
  property $ do
    str <-
      forAll $
      Gen.list (Range.constant 0 5) $
      Gen.string (Range.constant 0 5) Gen.unicode
    Data.List.permutations str === Consy.permutations str

prop_inits_correct :: Property
prop_inits_correct =
  property $ do
    str <-
      forAll $
      Gen.list (Range.constant 0 10) $
      Gen.string (Range.constant 0 10) Gen.unicode
    Data.List.inits str === Consy.inits str
    Data.List.inits str === listInits str

main :: IO Bool
main = checkParallel $$(discover)
