{- Inspection tests for
== Accumulating maps ==
mapAccumL
mapAccumR
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.AccumulatingMaps where

import Test.Inspection ((===), inspect)

import Data.Char (Char)

import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy

consMapAccumLList, mapAccumLList :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
consMapAccumLList = mapAccumL
mapAccumLList = Data.List.mapAccumL

inspect ('consMapAccumLList === 'mapAccumLList)

consMapAccumLText, mapAccumLText
  :: (a -> Char -> (a, Char))
  -> a
  -> Data.Text.Text
  -> (a, Data.Text.Text)
consMapAccumLText = mapAccumL
mapAccumLText = Data.Text.mapAccumL

inspect ('consMapAccumLText === 'mapAccumLText)

consMapAccumLLazyText, mapAccumLLazyText
  :: (a -> Char -> (a, Char))
  -> a
  -> Data.Text.Lazy.Text
  -> (a, Data.Text.Lazy.Text)
consMapAccumLLazyText = mapAccumL
mapAccumLLazyText = Data.Text.Lazy.mapAccumL

inspect ('consMapAccumLLazyText === 'mapAccumLLazyText)

consMapAccumLVector, mapAccumLVector
  :: (a -> b -> (a, c))
  -> a
  -> Data.Vector.Vector b
  -> (a, Data.Vector.Vector c)
consMapAccumLVector = mapAccumL
mapAccumLVector = Data.List.mapAccumL

inspect ('consMapAccumLVector === 'mapAccumLVector)


consMapAccumRList, mapAccumRList :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
consMapAccumRList = mapAccumR
mapAccumRList = Data.List.mapAccumR

inspect ('consMapAccumRList === 'mapAccumRList)

consMapAccumRText, mapAccumRText
  :: (a -> Char -> (a, Char))
  -> a
  -> Data.Text.Text
  -> (a, Data.Text.Text)
consMapAccumRText = mapAccumR
mapAccumRText = Data.Text.mapAccumR

inspect ('consMapAccumRText === 'mapAccumRText)

consMapAccumRLazyText, mapAccumRLazyText
  :: (a -> Char -> (a, Char))
  -> a
  -> Data.Text.Lazy.Text
  -> (a, Data.Text.Lazy.Text)
consMapAccumRLazyText = mapAccumR
mapAccumRLazyText = Data.Text.Lazy.mapAccumR

inspect ('consMapAccumRLazyText === 'mapAccumRLazyText)

consMapAccumRVector, mapAccumRVector
  :: (a -> b -> (a, c))
  -> a
  -> Data.Vector.Vector b
  -> (a, Data.Vector.Vector c)
consMapAccumRVector = mapAccumR
mapAccumRVector = Data.List.mapAccumR

inspect ('consMapAccumRVector === 'mapAccumRVector)
