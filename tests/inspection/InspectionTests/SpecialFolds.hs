{- Inspection tests for
== Special folds ==
+ concat
+ concatMap
+ and
+ or
+ any
+ all
+ sum
+ product
+ maximum
+ minimum
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin -ddump-to-file -ddump-simpl -ddump-simpl-stats #-}
module InspectionTests.SpecialFolds where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..), (<), (>))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.List (errorEmptyList)
import GHC.Num (Num, (+), (-), (*))
import GHC.Real (fromIntegral)
import Test.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Internal.Fusion
import qualified Data.Vector
import qualified Data.Word
import Consy


{- concat -}
consConcat, listConcat :: [[a]] -> [a]
consConcat = concat
listConcat = foldr (append) []
inspect ('consConcat === 'listConcat)

consConcatText, textConcat :: [Text] -> Text
consConcatText = concat
textConcat = Data.Text.concat
inspect ('consConcatText === 'textConcat)

consConcatLazyText, lazyTextConcat :: [Data.Text.Lazy.Text] -> Data.Text.Lazy.Text
consConcatLazyText = concat
lazyTextConcat = Data.Text.Lazy.concat
inspect ('consConcatLazyText === 'lazyTextConcat)

consConcatVector, vectorConcat :: [Vector a] -> Vector a
consConcatVector = concat
vectorConcat = Data.Vector.concat
inspect ('consConcatVector === 'vectorConcat)

consConcatBS, bsConcat :: [Data.ByteString.ByteString] -> Data.ByteString.ByteString
consConcatBS = concat
bsConcat = Data.ByteString.concat
inspect ('consConcatBS === 'bsConcat)

consConcatLBS, lbsConcat :: [Data.ByteString.Lazy.ByteString] -> Data.ByteString.Lazy.ByteString
consConcatLBS = concat
lbsConcat = Data.ByteString.Lazy.concat
inspect ('consConcatLBS === 'lbsConcat)


{- concatMap -}
consConcatMap, listConcatMap :: (a -> [b]) -> [a] -> [b]
consConcatMap = concatMap
listConcatMap f = \as -> foldr ((append) . f) [] as
inspect ('consConcatMap === 'listConcatMap)

consConcatMapText, textConcatMap :: (Char -> Text) -> Text -> Text
consConcatMapText = concatMap
textConcatMap = Data.Text.concatMap
inspect ('consConcatMapText === 'textConcatMap)

consConcatMapLazyText, lazyTextConcatMap :: (Char -> Data.Text.Lazy.Text) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consConcatMapLazyText = concatMap
lazyTextConcatMap = Data.Text.Lazy.concatMap
inspect ('consConcatMapLazyText === 'lazyTextConcatMap)

consConcatMapVector, vectorConcatMap :: (a -> Vector b) -> Vector a -> Vector b
consConcatMapVector = concatMap
vectorConcatMap = Data.Vector.concatMap
inspect ('consConcatMapVector === 'vectorConcatMap)

consConcatMapBS, bsConcatMap :: (Word8 -> Data.ByteString.ByteString) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consConcatMapBS = concatMap
bsConcatMap = Data.ByteString.concatMap
inspect ('consConcatMapBS === 'bsConcatMap)

consConcatMapLBS, lbsConcatMap :: (Word8 -> Data.ByteString.Lazy.ByteString) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consConcatMapLBS = concatMap
lbsConcatMap = Data.ByteString.Lazy.concatMap
inspect ('consConcatMapLBS === 'lbsConcatMap)


{- and -}
consAnd, listAnd :: [Bool] -> Bool
consAnd = and
listAnd = foldr (&&) True
inspect ('consAnd === 'listAnd)

consAndVector, vectorAnd :: Vector Bool -> Bool
consAndVector = and
vectorAnd = Data.Vector.and
inspect ('consAndVector === 'vectorAnd)


{- or -}
consOr, listOr :: [Bool] -> Bool
consOr = or
listOr = foldr (||) False
inspect ('consOr === 'listOr)

consOrVector, vectorOr :: Vector Bool -> Bool
consOrVector = or
vectorOr = Data.Vector.or
inspect ('consOrVector === 'vectorOr)


{- any -}
-- consAny, listAny :: Foldable t => (a -> Bool) -> t a -> Bool
consAny, listAny :: (a -> Bool) -> [a] -> Bool
consAny = any
listAny p = go
  where
  go s =
    case s of
      [] -> False
      (x:xs) -> p x || go xs
inspect ('consAny === 'listAny)

consAnyText, textAny :: (Char -> Bool) -> Text -> Bool
consAnyText = any
textAny = Data.Text.any
inspect ('consAnyText === 'textAny)

consAnyLazyText, lazyTextAny :: (Char -> Bool) -> Data.Text.Lazy.Text -> Bool
consAnyLazyText = any
lazyTextAny = Data.Text.Lazy.any
inspect ('consAnyLazyText === 'lazyTextAny)

consAnyVector, vectorAny :: (a -> Bool) -> Vector a -> Bool
consAnyVector = any
vectorAny = Data.Vector.any
inspect ('consAnyVector === 'vectorAny)

consAnyBS, bsAny :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Bool
consAnyBS = any
bsAny = Data.ByteString.any
inspect ('consAnyBS === 'bsAny)

consAnyLBS, lbsAny :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Bool
consAnyLBS = any
lbsAny = Data.ByteString.Lazy.any
inspect ('consAnyLBS === 'lbsAny)


{- all -}
-- consAll, listAll :: Foldable t => (a -> Bool) -> t a -> Bool
consAll, listAll :: (a -> Bool) -> [a] -> Bool
consAll = all
listAll p = go
  where
  go s =
    case s of
      [] -> True
      (x:xs) -> p x && go xs
inspect ('consAll === 'listAll)

consAllText, textAll :: (Char -> Bool) -> Text -> Bool
consAllText = all
textAll = Data.Text.all
inspect ('consAllText === 'textAll)

consAllLazyText, lazyTextAll :: (Char -> Bool) -> Data.Text.Lazy.Text -> Bool
consAllLazyText = all
lazyTextAll = Data.Text.Lazy.all
inspect ('consAllLazyText === 'lazyTextAll)

consAllVector, vectorAll :: (a -> Bool) -> Vector a -> Bool
consAllVector = all
vectorAll = Data.Vector.all
inspect ('consAllVector === 'vectorAll)

consAllBS, bsAll :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Bool
consAllBS = all
bsAll = Data.ByteString.all
inspect ('consAllBS === 'bsAll)

consAllLBS, lbsAll :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Bool
consAllLBS = all
lbsAll = Data.ByteString.Lazy.all
inspect ('consAllLBS === 'lbsAll)


{- sum -}
consSum, listSum :: Num a => [a] -> a
consSum = sum
-- listSum = foldl (+) 0
listSum = go
  where
  go s =
    case s of
      [] -> 0
      (x:xs) -> x + go xs
inspect ('consSum === 'listSum)

-- -- FAILS
-- consSumVector, vectorSum :: Num a => Vector a -> a
-- consSumVector = sum
-- vectorSum = Data.Vector.sum
-- inspect ('consSumVector === 'vectorSum)

-- FAILS
-- consSumSeq, seqSum :: Num a => Data.Sequence.Seq a -> a
-- consSumSeq = sum
-- seqSum = Data.Foldable.sum
-- inspect ('consSumSeq === 'seqSum)


{- product -}
consProduct, listProduct :: Num a => [a] -> a
consProduct = product
-- listProduct = foldl (*) 1
listProduct = go
  where
  go s =
    case s of
      [] -> 1
      (x:xs) -> x * go xs
inspect ('consProduct === 'listProduct)

-- FAILS
-- consProductVector, vectorProduct :: Num a => Vector a -> a
-- consProductVector = product
-- vectorProduct = Data.Vector.product
-- inspect ('consProductVector === 'vectorProduct)

-- FAILS
-- consProductSeq, seqProduct :: Num a => Data.Sequence.Seq a -> a
-- consProductSeq = product
-- seqProduct = Data.Foldable.product
-- inspect ('consProductSeq === 'seqProduct)


{- maximum -}
consMaximum, listMaximum :: (Ord a) => [a] -> a
consMaximum = maximum
listMaximum = go
  where
  go s =
    case s of
      [] -> errorEmptyList "maximum"
      otherwise -> foldl1 max s
inspect ('consMaximum === 'listMaximum)

consMaximumText, textMaximum :: Text -> Char
consMaximumText = maximum
textMaximum = Data.Text.maximum
inspect ('consMaximumText === 'textMaximum)

consMaximumLazyText, lazyTextMaximum :: Data.Text.Lazy.Text -> Char
consMaximumLazyText = maximum
lazyTextMaximum = Data.Text.Lazy.maximum
inspect ('consMaximumLazyText === 'lazyTextMaximum)

-- FAILS
-- consMaximumVector, vectorMaximum :: (Ord a) => Vector a -> a
-- consMaximumVector = maximum
-- vectorMaximum = Data.Vector.maximum
-- inspect ('consMaximumVector === 'vectorMaximum)

consMaximumBS, bsMaximum :: Data.ByteString.ByteString -> Word8
consMaximumBS = maximum
bsMaximum = Data.ByteString.maximum
inspect ('consMaximumBS === 'bsMaximum)

consMaximumLBS, lbsMaximum :: Data.ByteString.Lazy.ByteString -> Word8
consMaximumLBS = maximum
lbsMaximum = Data.ByteString.Lazy.maximum
inspect ('consMaximumLBS === 'lbsMaximum)

-- FAILS
-- consMaximumSeq, seqMaximum :: (Ord a) => Data.Sequence.Seq a -> a
-- consMaximumSeq = maximum
-- seqMaximum = Data.Foldable.maximum
-- inspect ('consMaximumSeq === 'seqMaximum)


{- minimum -}
consMinimum, listMinimum :: (Ord a) => [a] -> a
consMinimum = minimum
listMinimum = go
  where
  go s =
    case s of
      [] -> errorEmptyList "minimum"
      otherwise -> foldl1 min s
inspect ('consMinimum === 'listMinimum)

consMinimumText, textMinimum :: Text -> Char
consMinimumText = minimum
textMinimum = Data.Text.minimum
inspect ('consMinimumText === 'textMinimum)

consMinimumLazyText, lazyTextMinimum :: Data.Text.Lazy.Text -> Char
consMinimumLazyText = minimum
lazyTextMinimum = Data.Text.Lazy.minimum
inspect ('consMinimumLazyText === 'lazyTextMinimum)

-- FAILS
-- consMinimumVector, vectorMinimum :: (Ord a) => Vector a -> a
-- consMinimumVector = minimum
-- vectorMinimum = Data.Vector.minimum
-- inspect ('consMinimumVector === 'vectorMinimum)

consMinimumBS, bsMinimum :: Data.ByteString.ByteString -> Word8
consMinimumBS = minimum
bsMinimum = Data.ByteString.minimum
inspect ('consMinimumBS === 'bsMinimum)

consMinimumLBS, lbsMinimum :: Data.ByteString.Lazy.ByteString -> Word8
consMinimumLBS = minimum
lbsMinimum = Data.ByteString.Lazy.minimum
inspect ('consMinimumLBS === 'lbsMinimum)

-- FAILS
-- consMinimumSeq, seqMinimum :: (Ord a) => Data.Sequence.Seq a -> a
-- consMinimumSeq = minimum
-- seqMinimum = Data.Foldable.minimum
-- inspect ('consMinimumSeq === 'seqMinimum)
