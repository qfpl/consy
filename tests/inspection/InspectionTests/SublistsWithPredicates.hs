{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.SublistsWithPredicates where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.), id)
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord ((<), (>))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (IO, pure, return)
import GHC.Enum (succ)
import GHC.List (elem,errorEmptyList)
import GHC.Num (Num, Integer, (+), (-))
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


{- isPrefixOf -}
consIsPrefixOfList, listIsPrefixOf :: Eq a => [a] -> [a] -> Bool
consIsPrefixOfList = isPrefixOf
listIsPrefixOf [] _ = True
listIsPrefixOf _ [] = False
listIsPrefixOf (x:xs) (y:ys) = x == y && listIsPrefixOf xs ys
{-
isPrefixOf compiles to slightly different core, but both implementations
have the same performance

inspect ('consIsPrefixOfList === 'listIsPrefixOf)
-}

consIsPrefixOfText, textIsPrefixOf :: Text -> Text -> Bool
consIsPrefixOfText = isPrefixOf
textIsPrefixOf = Data.Text.isPrefixOf
inspect ('consIsPrefixOfText === 'textIsPrefixOf)

consIsPrefixOfLazyText, lazyTextIsPrefixOf :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Bool
consIsPrefixOfLazyText = isPrefixOf
lazyTextIsPrefixOf = Data.Text.Lazy.isPrefixOf
inspect ('consIsPrefixOfLazyText === 'lazyTextIsPrefixOf)

consIsPrefixOfBS, bsIsPrefixOf :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Bool
consIsPrefixOfBS = isPrefixOf
bsIsPrefixOf = Data.ByteString.isPrefixOf
inspect ('consIsPrefixOfBS === 'bsIsPrefixOf)

consIsPrefixOfLBS, lbsIsPrefixOf :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> Bool
consIsPrefixOfLBS = isPrefixOf
lbsIsPrefixOf = Data.ByteString.Lazy.isPrefixOf
inspect ('consIsPrefixOfLBS === 'lbsIsPrefixOf)


{- isSuffixOf -}
consIsSuffixOfList, listIsSuffixOf :: Eq a => [a] -> [a] -> Bool
consIsSuffixOfList = isSuffixOf
listIsSuffixOf ns hs =
  maybe False id $ do
    delta <- dropLengthMaybe ns hs
    return $ ns == dropLength delta hs
  where
    dropLength a b =
      case a of
        [] -> b
        _:xs ->
          case b of
            [] -> []
            _:ys -> dropLength xs ys

    dropLengthMaybe a b =
      case a of
        [] -> Just b
        _:xs ->
          case b of
            [] -> Nothing
            _:ys -> dropLengthMaybe xs ys
{-
inspect ('consIsSuffixOfList === 'listIsSuffixOf)

This inspection test fails due to implementation differences, but the benchmarks
show it is just as fast as the Data.List version
-}

consIsSuffixOfText, textIsSuffixOf :: Text -> Text -> Bool
consIsSuffixOfText = isSuffixOf
textIsSuffixOf = Data.Text.isSuffixOf
inspect ('consIsSuffixOfText === 'textIsSuffixOf)

consIsSuffixOfLazyText, lazyTextIsSuffixOf :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Bool
consIsSuffixOfLazyText = isSuffixOf
lazyTextIsSuffixOf = Data.Text.Lazy.isSuffixOf
inspect ('consIsSuffixOfLazyText === 'lazyTextIsSuffixOf)

consIsSuffixOfBS, bsIsSuffixOf :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Bool
consIsSuffixOfBS = isSuffixOf
bsIsSuffixOf = Data.ByteString.isSuffixOf
inspect ('consIsSuffixOfBS === 'bsIsSuffixOf)

consIsSuffixOfLBS, lbsIsSuffixOf :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> Bool
consIsSuffixOfLBS = isSuffixOf
lbsIsSuffixOf = Data.ByteString.Lazy.isSuffixOf
inspect ('consIsSuffixOfLBS === 'lbsIsSuffixOf)


{- isInfixOf -}
consIsInfixOfList, listIsInfixOf :: forall a. Eq a => [a] -> [a] -> Bool
consIsInfixOfList = isInfixOf
listIsInfixOf needle haystack =
  any (Data.List.isPrefixOf needle) (Data.List.tails haystack)
{-
isInfixOf compiles to different core for the same reason that isPrefixOf does, but
they are the same speed in practise

inspect ('consIsInfixOfList === 'listIsInfixOf)
-}

consIsInfixOfText, textIsInfixOf :: Text -> Text -> Bool
consIsInfixOfText = isInfixOf
textIsInfixOf = Data.Text.isInfixOf
inspect ('consIsInfixOfText === 'textIsInfixOf)

consIsInfixOfLazyText, lazyTextIsInfixOf :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Bool
consIsInfixOfLazyText = isInfixOf
lazyTextIsInfixOf = Data.Text.Lazy.isInfixOf
inspect ('consIsInfixOfLazyText === 'lazyTextIsInfixOf)

consIsInfixOfBS, bsIsInfixOf :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Bool
consIsInfixOfBS = isInfixOf
bsIsInfixOf = Data.ByteString.isInfixOf
inspect ('consIsInfixOfBS === 'bsIsInfixOf)


{- isSubsequenceOf -}
consIsSubsequenceOfList, listIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
consIsSubsequenceOfList = isSubsequenceOf
listIsSubsequenceOf [] _ = True
listIsSubsequenceOf _ [] = False
listIsSubsequenceOf a@(x:a') (y:b)
  | x == y = listIsSubsequenceOf a' b
  | otherwise = listIsSubsequenceOf a b
{-
implementations are slightly different but performance is the same

inspect ('consIsSubsequenceOfList === 'listIsSubsequenceOf)
-}
