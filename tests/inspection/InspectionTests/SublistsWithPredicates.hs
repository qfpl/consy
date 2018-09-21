{- Inspection tests for
== Sublists (Predicates) ==
+ isPrefixOf
+ isSuffixOf
isInfixOf
+ isSubsequenceOf
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin -ddump-to-file -ddump-simpl -ddump-simpl-stats #-}
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
listIsPrefixOf = go
  where
    go [] _ = True
    go _ [] = False
    go (x:xs) (y:ys) = x == y && go xs ys
inspect ('consIsPrefixOfList === 'listIsPrefixOf)

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
-- -- FAILS
-- consIsSuffixOfList, listIsSuffixOf :: Eq a => [a] -> [a] -> Bool
-- consIsSuffixOfList = isSuffixOf
-- ns `listIsSuffixOf` hs =
--   maybe False id
--   (
--     do
--       delta <- dropLengthMaybe ns hs
--       return (ns == dropLength delta hs)
--   )
--   where
--     dropLength :: [a] -> [b] -> [b]
--     -- dropLength [] y = y
--     -- dropLength _ [] = []
--     -- dropLength (_:x') (_:y') = dropLength x' y'
--     dropLength = \a -> go a
--       where
--         go a b =
--           case a of
--             [] -> b
--             (_:as) -> case b of
--                         [] -> []
--                         (_:bs) -> go as bs
--
--     dropLengthMaybe :: [a] -> [b] -> Maybe [b]
--     -- dropLengthMaybe [] y = Just y
--     -- dropLengthMaybe _ [] = Nothing
--     -- dropLengthMaybe (_:x') (_:y') = dropLengthMaybe x' y'
--     dropLengthMaybe = \a -> go a
--       where
--         go a b =
--           case a of
--             [] -> Just b
--             (_:as) -> case b of
--                         [] -> Nothing
--                         (_:bs) -> go as bs
--
-- inspect ('consIsSuffixOfList === 'listIsSuffixOf)
--
-- consIsSuffixOfText, textIsSuffixOf :: Text -> Text -> Bool
-- consIsSuffixOfText = isSuffixOf
-- textIsSuffixOf = Data.Text.isSuffixOf
-- inspect ('consIsSuffixOfText === 'textIsSuffixOf)
--
-- consIsSuffixOfLazyText, lazyTextIsSuffixOf :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Bool
-- consIsSuffixOfLazyText = isSuffixOf
-- lazyTextIsSuffixOf = Data.Text.Lazy.isSuffixOf
-- inspect ('consIsSuffixOfLazyText === 'lazyTextIsSuffixOf)
--
-- consIsSuffixOfBS, bsIsSuffixOf :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Bool
-- consIsSuffixOfBS = isSuffixOf
-- bsIsSuffixOf = Data.ByteString.isSuffixOf
-- inspect ('consIsSuffixOfBS === 'bsIsSuffixOf)
--
-- consIsSuffixOfLBS, lbsIsSuffixOf :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> Bool
-- consIsSuffixOfLBS = isSuffixOf
-- lbsIsSuffixOf = Data.ByteString.Lazy.isSuffixOf
-- inspect ('consIsSuffixOfLBS === 'lbsIsSuffixOf)


{- isInfixOf -}
-- FAILS
consIsInfixOfList, listIsInfixOf :: forall a. Eq a => [a] -> [a] -> Bool
consIsInfixOfList = isInfixOf
-- listIsInfixOf = go
--   where
--     go [] _ = True
--     go _ [] = False
--     go (x:xs) (y:ys) = x == y && go xs ys
listIsInfixOf needle haystack = Data.List.elem needle (Data.List.tails haystack)
-- listIsInfixOf needle haystack =
--   case mayContains of
--     Nothing -> False
--     Just _ -> True
--   where
--     mayContains :: forall a. Maybe [a]
--     mayContains = Data.List.find (Data.List.isPrefixOf needle :: [a] -> Bool) (Data.List.tails haystack :: [[a]])
-- inspect ('consIsInfixOfList === 'listIsInfixOf)

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
-- -- FAILS
-- consIsSubsequenceOfList, listIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
-- consIsSubsequenceOfList = isSubsequenceOf
-- listIsSubsequenceOf = \s t -> go s t
--   where
--     go s t =
--       case s of
--         [] -> True
--         (s':ss') -> case t of
--                       [] -> False
--                       (t':tt')
--                         | s' == t' -> go ss' tt'
--                         | otherwise -> go s tt'
--     -- go [] _ = True
--     -- go _ [] = False
--     -- go a@(x:a') (y:b)
--     --   | x == y    = go a' b
--     --   | otherwise = go a b
-- inspect ('consIsSubsequenceOfList === 'listIsSubsequenceOf)
