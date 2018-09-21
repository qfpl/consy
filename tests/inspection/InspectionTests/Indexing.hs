{- Inspection tests for
== Indexing Sublists ==
+ (!!)
+ elemIndex
+ elemIndices
+ findIndex
+ findIndices
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin -ddump-to-file -ddump-simpl -ddump-simpl-stats #-}
module InspectionTests.Indexing where

import Data.Bool (Bool(..), otherwise)
import Data.Char (Char)
import Data.Eq (Eq(..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..), listToMaybe)
import Data.Ord ((<))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (Int( I# ), (+#), errorWithoutStackTrace)
import GHC.Num ((-))
import GHC.Real (fromIntegral)
import Test.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector
import qualified Data.Word
import Consy


{- (!!) -}
-- FAILS
consListIndex, listIndex :: [a] -> Int -> a
consListIndex = (!!)
-- listIndex ls n
--       | n < 0  =  errorWithoutStackTrace "Prelude.!!: negative index"
--       | otherwise = go ls n
--           where
--             go [] _ =  errorWithoutStackTrace "Prelude.!!: index too large"
--             go (x:_) 0 =  x
--             go (_:xs) n =  go xs (n-1)
listIndex =  go
  where
    go ls !n
      | n < 0 = errorWithoutStackTrace "Prelude.!!: negative index"
      | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) tooLarge ls n
tooLarge :: Int -> a
tooLarge _ = errorWithoutStackTrace "Prelude.!!: index too large"
-- inspect ('consListIndex === 'listIndex)


{- elemIndex -}
consElemIndex, listElemIndex :: Eq a => a -> [a] -> Maybe Int
consElemIndex x xs = elemIndex x xs
listElemIndex x xs = listToMaybe (Data.List.findIndices (x==) xs)
inspect ('consElemIndex === 'listElemIndex)

consElemIndexVector, vectorElemIndex :: Eq a => a -> Vector a -> Maybe Int
consElemIndexVector = elemIndex
vectorElemIndex = Data.Vector.elemIndex
inspect ('consElemIndexVector === 'vectorElemIndex)

consElemIndexBS, bsElemIndex :: Word8 -> Data.ByteString.ByteString -> Maybe Int
consElemIndexBS = elemIndex
bsElemIndex = Data.ByteString.elemIndex
inspect ('consElemIndexBS === 'bsElemIndex)

consElemIndexLBS, lbsElemIndex :: Word8 -> Data.ByteString.Lazy.ByteString -> Maybe Int64
consElemIndexLBS x xs = fromIntegral <$> elemIndex x xs
lbsElemIndex x xs = Data.ByteString.Lazy.elemIndex x xs
inspect ('consElemIndexLBS === 'lbsElemIndex)


{- elemIndices -}
consElemIndices, listElemIndices :: Eq a => a -> [a] -> [Int]
consElemIndices x ls = elemIndices x ls
listElemIndices x ls = Data.List.findIndices (x==) ls
inspect ('consElemIndices === 'listElemIndices)

consElemIndicesBS, bsElemIndices :: Word8 -> Data.ByteString.ByteString -> [Int]
consElemIndicesBS = elemIndices
bsElemIndices = Data.ByteString.elemIndices
inspect ('consElemIndicesBS === 'bsElemIndices)

consElemIndicesLBS, lbsElemIndices :: Word8 -> Data.ByteString.Lazy.ByteString -> [Int64]
consElemIndicesLBS p ls = fromIntegral <$> elemIndices p ls
lbsElemIndices = Data.ByteString.Lazy.elemIndices
inspect ('consElemIndicesLBS === 'lbsElemIndices)


{- findIndex -}
consFindIndex, listFindIndex :: (a -> Bool) -> [a] -> Maybe Int
consFindIndex p ls = findIndex p ls
listFindIndex p = listToMaybe . Data.List.findIndices p
inspect ('consFindIndex === 'listFindIndex)

consFindIndexText, textFindIndex :: (Char -> Bool) -> Text -> Maybe Int
consFindIndexText = findIndex
textFindIndex = Data.Text.findIndex
inspect ('consFindIndexText === 'textFindIndex)

consFindIndexVector, vectorFindIndex :: (a -> Bool) -> Vector a -> Maybe Int
consFindIndexVector = findIndex
vectorFindIndex = Data.Vector.findIndex
inspect ('consFindIndexVector === 'vectorFindIndex)

consFindIndexBS, bsFindIndex :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Maybe Int
consFindIndexBS = findIndex
bsFindIndex = Data.ByteString.findIndex
inspect ('consFindIndexBS === 'bsFindIndex)

consFindIndexLBS, lbsFindIndex :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Maybe Int64
consFindIndexLBS p ls = fromIntegral <$> findIndex p ls
lbsFindIndex p ls = fromIntegral <$> Data.ByteString.Lazy.findIndex p ls
inspect ('consFindIndexLBS === 'lbsFindIndex)


{- findIndices -}
consFindIndices, listFindIndices :: (a -> Bool) -> [a] -> [Int]
consFindIndices p ls = findIndices p ls
listFindIndices p ls = build $ \c n ->
  let go x r k | p x       = I# k `c` r (k +# 1#)
               | otherwise = r (k +# 1#)
  in foldr go (\_ -> n) ls 0#
inspect ('consFindIndices === 'listFindIndices)

consFindIndicesBS, bsFindIndices :: (Word8 -> Bool) -> Data.ByteString.ByteString -> [Int]
consFindIndicesBS = findIndices
bsFindIndices = Data.ByteString.findIndices
inspect ('consFindIndicesBS === 'bsFindIndices)

consFindIndicesLBS, lbsFindIndices :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> [Int64]
consFindIndicesLBS p ls = fromIntegral <$> findIndices p ls
lbsFindIndices = Data.ByteString.Lazy.findIndices
inspect ('consFindIndicesLBS === 'lbsFindIndices)

-- consFindIndicesSeq, seqFindIndices :: (a -> Bool) -> Vector a -> Vector Int
-- consFindIndicesSeq = findIndices
-- seqFindIndices = Data.Vector.findIndices
-- inspect ('consFindIndicesSeq === 'seqFindIndices)
