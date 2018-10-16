{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Consy.Folds
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , augment
  , build
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr1
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Char (Char)
import Data.Int (Int)
import Data.Function (id)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Base (oneShot, seq)
import GHC.List (errorEmptyList)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Foldable
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector


{-# inline [1] augment #-}
-- augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
augment :: Cons s s a a
  => (forall b. (a->b->b) -> b -> b)
      -> s
      -> s
augment g xs = g cons xs
{-# rules
"cons foldr/augment"
    forall k z xs (g::forall b. (a->b->b) -> b -> b).
    foldr k z (augment g xs) = g k (foldr k z xs)
#-}


{-# inline [1] build #-}
-- build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
build :: (AsEmpty s, Cons s s a a)
  => (forall b. (a -> b -> b) -> b -> b)
      -> s
build g = g cons Empty
{-# rules
"cons fold/build"
    forall k z (g::forall b. (a->b->b) -> b -> b).
    foldr k z (build g) = g k z
#-}


{-# inline [0] foldl #-}
-- foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
foldl :: Cons s s a a => (b -> a -> b) -> b -> s -> b
foldl k z0 xs =
  foldr
    (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> fn (k z v)))
    (id :: b -> b)
    xs
    z0

{-# rules
"cons foldl text" [~2]
    foldl @Text @Char = Data.Text.foldl
"cons foldl text eta" [~2]
    forall f z xs.
    foldl @Text @Char f z xs = Data.Text.foldl f z xs

"cons foldl vector" [~2]
    foldl @(Vector _) = Data.Vector.foldl
"cons foldl vector eta" [~2]
    forall f z xs.
    foldl @(Vector _) f z xs = Data.Vector.foldl f z xs

"cons foldl bs" [~2]
    foldl @BS.ByteString = BS.foldl
"cons foldl bs eta" [~2]
    forall f z xs.
    foldl @BS.ByteString f z xs = BS.foldl f z xs

"cons foldl bslazy" [~2]
    foldl @LBS.ByteString = LBS.foldl
"cons foldl bslazy eta" [~2]
    forall f z xs.
    foldl @LBS.ByteString f z xs = LBS.foldl f z xs

"cons foldl seq" [~2]
    foldl @(Seq _) = Data.Foldable.foldl
"cons foldl seq eta" [~2]
    forall f z xs.
    foldl @(Seq _) f z xs = Data.Foldable.foldl f z xs
#-}


{-# inline [2] foldl' #-}
-- foldl' :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl' :: forall s a b. Cons s s a a => (b -> a -> b) -> b -> s -> b
foldl' k z0 xs =
  foldr
    (\(v :: a) (fn :: b -> b) -> oneShot (\(z :: b) -> z `seq` fn (k z v)))
    (id :: b -> b)
    xs
    z0

{-# rules
"cons foldl' text" [~2]
    foldl' @Text @Char = Data.Text.foldl'
"cons foldl' text eta" [~2]
    forall f z xs.
    foldl' @Text @Char f z xs = Data.Text.foldl' f z xs

"cons foldl' vector" [~2]
    foldl' @(Vector _) = Data.Vector.foldl'
"cons foldl' vector eta" [~2]
    forall f z xs.
    foldl' @(Vector _) f z xs = Data.Vector.foldl' f z xs

"cons foldl' bs" [~2]
    foldl' @BS.ByteString = BS.foldl'
"cons foldl' bs eta" [~2]
    forall f z xs.
    foldl' @BS.ByteString f z xs = BS.foldl' f z xs

"cons foldl' bslazy" [~2]
    foldl' @LBS.ByteString = LBS.foldl'
"cons foldl' bslazy eta" [~2]
    forall f z xs.
    foldl' @LBS.ByteString f z xs = LBS.foldl' f z xs

"cons foldl' seq" [~2]
    foldl' @(Seq _) = Data.Foldable.foldl'
"cons foldl' seq eta" [~2]
    forall f z xs.
    foldl' @(Seq _) f z xs = Data.Foldable.foldl' f z xs
#-}


{-# inline [2] foldl1 #-}
-- foldl1 :: forall a. (a -> a -> a) -> [a] -> a
foldl1 :: forall s a. Cons s s a a => (a -> a -> a) -> s -> a
foldl1 k s =
  case uncons s of
    Nothing -> errorEmptyList "foldl1"
    Just (a, as) -> foldl k a as

{-# rules
"cons foldl1 text" [~2]
    foldl1 @Text @Char = Data.Text.foldl1
"cons foldl1 text eta" [~2]
    forall f xs.
    foldl1 @Text @Char f xs = Data.Text.foldl1 f xs

"cons foldl1 vector" [~2]
    foldl1 @(Vector _) = Data.Vector.foldl1
"cons foldl1 vector eta" [~2]
    forall f xs.
    foldl1 @(Vector _) f xs = Data.Vector.foldl1 f xs

"cons foldl1 bs" [~2]
    foldl1 @BS.ByteString = BS.foldl1
"cons foldl1 bs eta" [~2]
    forall f xs.
    foldl1 @BS.ByteString f xs = BS.foldl1 f xs

"cons foldl1 bslazy" [~2]
    foldl1 @LBS.ByteString = LBS.foldl1
"cons foldl1 bslazy eta" [~2]
    forall f xs.
    foldl1 @LBS.ByteString f xs = LBS.foldl1 f xs

"cons foldl1 seq" [~2]
    foldl1 @(Seq _) = Data.Foldable.foldl1
"cons foldl1 seq eta" [~2]
    forall f xs.
    foldl1 @(Seq _) f xs = Data.Foldable.foldl1 f xs
#-}


{-# inline [2] foldl1' #-}
-- foldl1' :: forall a. (a -> a -> a) -> [a] -> a
foldl1' :: forall s a. Cons s s a a => (a -> a -> a) -> s -> a
foldl1' k s =
  case uncons s of
    Nothing -> errorEmptyList "foldl1'"
    Just (a, as) -> foldl' k a as

{-# rules
"cons foldl1' text" [~2]
    foldl1' @Text @Char = Data.Text.foldl1'
"cons foldl1' text eta" [~2]
    forall f xs.
    foldl1' @Text @Char f xs = Data.Text.foldl1' f xs

"cons foldl1' vector" [~2]
    foldl1' @(Vector _) = Data.Vector.foldl1'
"cons foldl1' vector eta" [~2]
    forall f xs.
    foldl1' @(Vector _) f xs = Data.Vector.foldl1' f xs

"cons foldl1' bs" [~2]
    foldl1' @BS.ByteString = BS.foldl1'
"cons foldl1' bs eta" [~2]
    forall f xs.
    foldl1' @BS.ByteString f xs = BS.foldl1' f xs

"cons foldl1' bslazy" [~2]
    foldl1' @LBS.ByteString = LBS.foldl1'
"cons foldl1' bslazy eta" [~2]
    forall f xs.
    foldl1' @LBS.ByteString f xs = LBS.foldl1' f xs
#-}


{-# inline [0] foldr #-}
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr :: Cons s s a a => (a -> b -> b) -> b -> s -> b
foldr f z = go
  where
    go s =
      case uncons s of
        Nothing -> z
        Just (x, xs) -> f x (go xs)

{-# rules
"cons foldr/build" [2]
    forall f z (g :: forall b. (a -> b -> b) -> b -> b).
    foldr f z (build g) = g f z

"cons foldr text" [~2]
    foldr @Data.Text.Text = Data.Text.foldr
"cons foldr text eta" [~2]
    forall f z xs.
    foldr @Data.Text.Text f z xs = Data.Text.foldr f z xs

"cons foldr vector" [~2]
    foldr @(Vector _) = Data.Vector.foldr
"cons foldr vector eta" [~2]
    forall f z xs.
    foldr @(Vector _) f z xs = Data.Vector.foldr f z xs

"cons foldr ltext" [~2]
    foldr @Data.Text.Lazy.Text = Data.Text.Lazy.foldr
"cons foldr ltext eta" [~2]
    forall f z xs.
    foldr @Data.Text.Lazy.Text f z xs = Data.Text.Lazy.foldr f z xs

"cons foldr bs" [~2]
    foldr @BS.ByteString = BS.foldr
"cons foldr bs eta" [~2]
    forall f z xs.
    foldr @BS.ByteString f z xs = BS.foldr f z xs

"cons foldr lbs" [~2]
    foldr @LBS.ByteString = LBS.foldr
"cons foldr lbs eta" [~2]
    forall f z xs.
    foldr @LBS.ByteString f z xs = LBS.foldr f z xs

"cons foldr seq" [~2]
    foldr @(Seq _) = Data.Foldable.foldr
"cons foldr seq eta" [~2]
    forall f z xs.
    foldr @(Seq _) f z xs = Data.Foldable.foldr f z xs
#-}


{-# inline [0] foldr1 #-}
-- foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 :: (AsEmpty s, Cons s s a a) => (a -> a -> a) -> s -> a
foldr1 f = go
  where
    go s =
      case uncons s of
        Nothing -> errorEmptyList "foldr1"
        Just (x, Empty) -> x
        Just (x, xs) -> f x (go xs)

{-# rules
-- "cons foldr/build" [2]
--     forall f z (g :: forall b. (a -> b -> b) -> b -> b).
--     foldr1 f z (build g) = g f z

"cons foldr1 text" [~2]
    foldr1 @Data.Text.Text = Data.Text.foldr1
"cons foldr1 text eta" [~2]
    forall f xs.
    foldr1 @Data.Text.Text f xs = Data.Text.foldr1 f xs

"cons foldr1 vector" [~2]
    foldr1 @(Vector _) = Data.Vector.foldr1
"cons foldr vector eta" [~2]
    forall f xs.
    foldr1 @(Vector _) f xs = Data.Vector.foldr1 f xs

"cons foldr1 ltext" [~2]
    foldr1 @Data.Text.Lazy.Text = Data.Text.Lazy.foldr1
"cons foldr1 ltext eta" [~2]
    forall f xs.
    foldr1 @Data.Text.Lazy.Text f xs = Data.Text.Lazy.foldr1 f xs

"cons foldr1 bs" [~2]
    foldr1 @BS.ByteString = BS.foldr1
"cons foldr1 bs eta" [~2]
    forall f xs.
    foldr1 @BS.ByteString f xs = BS.foldr1 f xs

"cons foldr1 lbs" [~2]
    foldr1 @LBS.ByteString = LBS.foldr1
"cons foldr1 lbs eta" [~2]
    forall f xs.
    foldr1 @LBS.ByteString f xs = LBS.foldr1 f xs

"cons foldr1 seq" [~2]
    foldr1 @(Seq _) = Data.Foldable.foldr1
"cons foldr1 seq eta" [~2]
    forall f xs.
    foldr1 @(Seq _) f xs = Data.Foldable.foldr1 f xs
#-}
