{-
== Searching lists (Searching with a predicate) ==
+ find
+ filter
+ partition
-}

{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
-- {-# language BangPatterns #-}
-- {-# language PatternSynonyms #-}
-- {-# language RankNTypes #-}
-- {-# language ScopedTypeVariables #-}
module Consy.SearchingWithPredicate
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , filter
  , find
  , partition
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), (&&), not, otherwise)
import Data.Char (Char)
import Data.Function ((.))
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds (build, foldr)

{- ___ Searching lists (Searching with a predicate) _________________________ -}

{-# noinline [0] find #-}
-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find :: (AsEmpty s, Cons s s a a ) => (a -> Bool) -> s -> Maybe a
find f = go
  where
    go s =
      case uncons s of
        Nothing -> Nothing
        Just (x, xs)
          | f x       -> Just x
          | otherwise -> go xs

{-# rules
"cons find text"
    find @Text @Char = Data.Text.find
"cons find text eta"
    forall p xs.
    find @Text @Char p xs = Data.Text.find p xs

"cons find ltext"
    find @Data.Text.Lazy.Text @Char = Data.Text.Lazy.find
"cons find ltext eta"
    forall p xs.
    find @Data.Text.Lazy.Text @Char p xs = Data.Text.Lazy.find p xs

"cons find vector"
    find @(Vector _) = Data.Vector.find
"cons find vector eta"
    forall p xs.
    find @(Vector _) p xs = Data.Vector.find p xs

"cons find bs"
    find @BS.ByteString = BS.find
"cons find bs eta"
    forall p xs.
    find @BS.ByteString p xs = BS.find p xs

"cons find bslazy"
    find @LBS.ByteString = LBS.find
"cons find bslazy eta"
    forall p xs.
    find @LBS.ByteString p xs = LBS.find p xs
#-}

{-# noinline [0] filter #-}
-- filter :: (a -> Bool) -> [a] -> [a]
filter :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> s
filter f = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs)
          | f x -> x `cons` go xs
          | otherwise -> go xs

{-# inline [0] filterFB #-}
filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
filterFB c p x r
  | p x = x `c` r
  | otherwise = r

{-# rules
"cons filter" [~1]
    forall p xs.
    filter p xs = build (\c n -> foldr (filterFB c p) n xs)

"cons filterFB"
    forall c p q.
    filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)

"cons filterList" [1]
    forall p.
    foldr (filterFB (:) p) [] = filter p

"cons filter text"
    filter @Text @Char = Data.Text.filter
"cons filter text eta"
    forall p xs.
    filter @Text @Char p xs = Data.Text.filter p xs

"cons filter ltext"
    filter @Data.Text.Lazy.Text @Char = Data.Text.Lazy.filter
"cons filter ltext eta"
    forall p xs.
    filter @Data.Text.Lazy.Text @Char p xs = Data.Text.Lazy.filter p xs

"cons filter vector"
    filter @(Vector _) = Data.Vector.filter
"cons filter vector eta"
    forall p xs.
    filter @(Vector _) p xs = Data.Vector.filter p xs

"cons filter bs"
    filter @BS.ByteString = BS.filter
"cons filter bs eta"
    forall p xs.
    filter @BS.ByteString p xs = BS.filter p xs

"cons filter bslazy"
    filter @LBS.ByteString = LBS.filter
"cons filter bslazy eta"
    forall p xs.
    filter @LBS.ByteString p xs = LBS.filter p xs

"cons filter seq"
    filter @(Seq _) = Data.Sequence.filter
"cons filter seq eta"
    forall p xs.
    filter @(Seq _) p xs = Data.Sequence.filter p xs
#-}

{-# inline [2] partition #-}
-- partition :: (a -> Bool) -> [a] -> ([a], [a])
partition :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> (s, s)
partition = \p -> foldr (select p) (Empty, Empty)
  where
    select p x ~(ts,fs)
      | p x = (x `cons` ts,fs)
      | otherwise = (ts, x `cons` fs)

{-# rules
"cons partition text"
    partition @Text @Char = Data.Text.partition
"cons partition text eta"
    forall p xs.
    partition @Text @Char p xs = Data.Text.partition p xs

"cons partition ltext"
    partition @Data.Text.Lazy.Text @Char = Data.Text.Lazy.partition
"cons partition ltext eta"
    forall p xs.
    partition @Data.Text.Lazy.Text @Char p xs = Data.Text.Lazy.partition p xs

"cons partition vector"
    partition @(Vector _) = Data.Vector.partition
"cons partition vector eta"
    forall p xs.
    partition @(Vector _) p xs = Data.Vector.partition p xs

"cons partition bs"
    partition @BS.ByteString = BS.partition
"cons partition bs eta"
    forall p xs.
    partition @BS.ByteString p xs = BS.partition p xs

"cons partition bslazy"
    partition @LBS.ByteString = LBS.partition
"cons partition bslazy eta"
    forall p xs.
    partition @LBS.ByteString p xs = LBS.partition p xs

"cons partition seq"
    partition @(Seq _) = Data.Sequence.partition
"cons partition seq eta"
    forall p xs.
    partition @(Seq _) p xs = Data.Sequence.partition p xs
#-}
