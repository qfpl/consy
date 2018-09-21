-- {-# language BangPatterns #-}
-- {-# language NoImplicitPrelude #-}
-- {-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin -ddump-to-file -ddump-simpl #-}
module InspectionTests where

import  InspectionTests.Basic
-- import  InspectionTests.Transformations
-- import  InspectionTests.Folds
-- import  InspectionTests.SpecialFolds
-- import  InspectionTests.AccumulatingMaps
-- import  InspectionTests.InfiniteLists
-- import  InspectionTests.Unfolding
-- import  InspectionTests.ExtractingSublists
-- import  InspectionTests.SublistsWithPredicates
-- import  InspectionTests.SearchingByEquality
-- import  InspectionTests.SearchingWithPredicate
-- import  InspectionTests.Indexing
-- import  InspectionTests.Zipping

import Consy
import Data.Text (Text)

-- main :: IO ()
-- main = pure ()
  -- print $ Consy.take 10 (Consy.repeat 'a' :: Text)
