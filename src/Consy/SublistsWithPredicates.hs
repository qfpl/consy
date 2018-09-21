{-
== Sublists (Predicates) ==
+ isPrefixOf
+ isSuffixOf
+ isInfixOf
+ isSubsequenceOf
-}

{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
-- {-# language BangPatterns #-}
-- {-# language PatternSynonyms #-}
module Consy.SublistsWithPredicates
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , isSubsequenceOf
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), (&&), otherwise)
import Data.Eq (Eq(..))
import Data.Function (id)
import Data.Maybe (Maybe(..), maybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Base (return)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.ExtractingSublists (tails)
import Consy.SearchingWithPredicate (find)
-- import Consy.Folds (foldr)
-- import Consy.SpecialFolds (any)
-- import Consy.TransformationsMap (map)
-- import Consy.SearchingByEquality (elem)


{- ___ Sublists (Predicates) ________________________________________________ -}

{-# inline [2] isPrefixOf #-}
-- isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf :: (AsEmpty s, Cons s s a a, Eq a) => s -> s -> Bool
isPrefixOf = go
  where
    go s t = case uncons s of
      Empty -> True
      Just (s',ss') -> case uncons t of
                        Empty -> False
                        Just (t', ts') -> s' == t' && go ss' ts'

{-# rules
"cons isPrefixOf text"
    isPrefixOf @Text = Data.Text.isPrefixOf
"cons isPrefixOf text eta"
    forall xs ys.
    isPrefixOf @Text xs ys = Data.Text.isPrefixOf xs ys

"cons isPrefixOf ltext"
    isPrefixOf @Data.Text.Lazy.Text = Data.Text.Lazy.isPrefixOf
"cons isPrefixOf ltext eta"
    forall xs ys.
    isPrefixOf @Data.Text.Lazy.Text xs ys = Data.Text.Lazy.isPrefixOf xs ys

"cons isPrefixOf bs"
    isPrefixOf @BS.ByteString = BS.isPrefixOf
"cons isPrefixOf bs eta"
    forall xs ys.
    isPrefixOf @BS.ByteString xs ys = BS.isPrefixOf xs ys

"cons isPrefixOf bslazy"
    isPrefixOf @LBS.ByteString = LBS.isPrefixOf
"cons isPrefixOf bslazy eta"
    forall xs ys.
    isPrefixOf @LBS.ByteString xs ys = LBS.isPrefixOf xs ys
#-}


{-# inline [2] isSuffixOf #-}
-- isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf :: (AsEmpty s, Cons s s a a, Eq a, Eq s) => s -> s -> Bool
ns `isSuffixOf` hs =
  maybe False id (do
      delta <- dropLengthMaybe ns hs
      return (ns == dropLength delta hs)
      )
  where
-- {-# inline [2] dropLength #-}
-- dropLength :: [a] -> [b] -> [b]
    dropLength :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t b b) => s -> t -> t
    dropLength = go
      where
        go s t = case uncons s of
          Nothing -> t
          Just (_, ss') -> case uncons t of
                            Nothing -> Empty
                            Just (_, tt') -> go ss' tt'
-- dropLength [] y = y
-- dropLength _ [] = []
-- dropLength (_:x') (_:y') = dropLength x' y'

-- {-# inline [2] dropLengthMaybe #-}
-- dropLengthMaybe :: [a] -> [b] -> Maybe [b]
    dropLengthMaybe :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t b b) => s -> t -> Maybe t
    dropLengthMaybe = go
      where
        go s t = case uncons s of
          Nothing -> Just t
          Just (_, ss') -> case uncons t of
                            Nothing -> Nothing
                            Just (_, tt') -> go ss' tt'
-- dropLengthMaybe [] y = Just y
-- dropLengthMaybe _ [] = Nothing
-- dropLengthMaybe (_:x') (_:y') = dropLengthMaybe x' y'


{-# rules
"cons isSuffixOf text"
    isSuffixOf @Text = Data.Text.isSuffixOf
"cons isSuffixOf text eta"
    forall xs ys.
    isSuffixOf @Text xs ys = Data.Text.isSuffixOf xs ys

"cons isSuffixOf ltext"
    isSuffixOf @Data.Text.Lazy.Text = Data.Text.Lazy.isSuffixOf
"cons isSuffixOf ltext eta"
    forall xs ys.
    isSuffixOf @Data.Text.Lazy.Text xs ys = Data.Text.Lazy.isSuffixOf xs ys

"cons isSuffixOf bs"
    isSuffixOf @BS.ByteString = BS.isSuffixOf
"cons isSuffixOf bs eta"
    forall xs ys.
    isSuffixOf @BS.ByteString xs ys = BS.isSuffixOf xs ys

"cons isSuffixOf lbs"
    isSuffixOf @LBS.ByteString = LBS.isSuffixOf
"cons isSuffixOf lbs eta"
    forall xs ys.
    isSuffixOf @LBS.ByteString xs ys = LBS.isSuffixOf xs ys
#-}


{-# inline [2] isInfixOf #-}
-- isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf :: forall s a. (AsEmpty s, Cons s s a a, Eq a) => s -> s -> Bool
isInfixOf needle haystack = case mayContains of
  Nothing -> False
  Just _ -> True
  where
    mayContains :: Maybe s
    mayContains = find (isPrefixOf needle) (tails haystack :: [s])
-- isInfixOf needle haystack = elem needle (tails haystack)

{-# rules
"cons isInfixOf text"
    isInfixOf @Text = Data.Text.isInfixOf
"cons isInfixOf text eta"
    forall xs ys.
    isInfixOf @Text xs ys = Data.Text.isInfixOf xs ys

"cons isInfixOf ltext"
    isInfixOf @Data.Text.Lazy.Text = Data.Text.Lazy.isInfixOf
"cons isInfixOf ltext eta"
    forall xs ys.
    isInfixOf @Data.Text.Lazy.Text xs ys = Data.Text.Lazy.isInfixOf xs ys

"cons isInfixOf bs"
    isInfixOf @BS.ByteString = BS.isInfixOf
"cons isInfixOf bs eta"
    forall xs ys.
    isInfixOf @BS.ByteString xs ys = BS.isInfixOf xs ys
#-}


{-# inline [2] isSubsequenceOf #-}
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf = \s t -> go s t
  where
    go s t = case uncons s of
              Nothing -> True
              Just (s', ss') -> case uncons t of
                          Nothing -> False
                          Just (t', tt')
                            | s' == t' -> go ss' tt'
                            | otherwise -> go s tt'
