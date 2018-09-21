{-
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

{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
-- {-# language BangPatterns #-}
-- {-# language PatternSynonyms #-}
-- {-# language RankNTypes #-}
-- {-# language ScopedTypeVariables #-}
module Consy.SpecialFolds
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , concat
  , concatMap
  , and
  , or
  , any
  , all
  , sum
  , product
  , maximum
  , minimum
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), (&&), (||))
import Data.Char (Char)
import Data.Function ((.))
import Data.Maybe (Maybe(..))
import Data.Ord (Ord, max, min)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.List (errorEmptyList)
import GHC.Num (Num, (+), (*))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Basic (append)
import Consy.Folds (build, foldl1, foldr)


{- ___ Special folds ________________________________________________________ -}

{-# noinline [1] concat #-}
-- concat :: [[a]] -> [a]
concat :: (AsEmpty s, AsEmpty t, Cons s s t t, Cons t t a a) => s -> t
concat = foldr (append) Empty

{-# rules
"cons concat" forall xs.
    concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)

"cons concat text" [~1]
    concat @[Text] = Data.Text.concat
"cons concat text eta" [~1]
    forall xs.
    concat @[Text] xs = Data.Text.concat xs

"cons concat ltext" [~1]
    concat @[Data.Text.Lazy.Text] = Data.Text.Lazy.concat
"cons concat ltext eta" [~1]
    forall xs.
    concat @[Data.Text.Lazy.Text] xs = Data.Text.Lazy.concat xs

"cons concat vector" [~1]
    concat @[(Vector _)] = Data.Vector.concat
"cons concat vector eta" [~1]
    forall xs.
    concat @[(Vector _)] xs = Data.Vector.concat xs

"cons concat bs" [~1]
    concat @[BS.ByteString] = BS.concat
"cons concat bs eta" [~1]
    forall xs.
    concat @[BS.ByteString] xs = BS.concat xs

"cons concat bslazy" [~1]
    concat @[LBS.ByteString] = LBS.concat
"cons concat bslazy eta" [~1]
    forall xs.
    concat @[LBS.ByteString] xs = LBS.concat xs
 #-}


{-# noinline [1] concatMap #-}
-- concatMap :: (a -> [b]) -> [a] -> [b]
concatMap :: (AsEmpty t, Cons s s a a, Cons t t b b) => (a -> t) -> s -> t
concatMap f = \as -> foldr (append . f) Empty as

{-# rules
"cons concatMap"
    forall f xs.
    concatMap f xs = build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)

"cons concatmap text" [~1]
    concatMap @Text = \f -> Data.Text.concatMap f
"cons concatMap text eta" [~1]
    forall f xs.
    concatMap @Text f xs = Data.Text.concatMap f xs

"cons concatMap ltext" [~1]
    concatMap @Data.Text.Lazy.Text = \f -> Data.Text.Lazy.concatMap f
"cons concatMap ltext eta" [~1]
    forall f xs.
    concatMap @Data.Text.Lazy.Text f xs = Data.Text.Lazy.concatMap f xs

"cons concatMap vector" [~1]
    concatMap @(Vector _) = \f -> Data.Vector.concatMap f
"cons concatMap vector eta" [~1]
    forall f xs.
    concatMap @(Vector _) f xs = Data.Vector.concatMap f xs

"cons concatMap bs" [~1]
    concatMap @BS.ByteString = \f -> BS.concatMap f
"cons concatMap bs eta" [~1]
    forall f xs.
    concatMap @BS.ByteString f xs = BS.concatMap f xs

"cons concatMap bslazy" [~1]
    concatMap @LBS.ByteString = \f -> LBS.concatMap f
"cons concatMap bslazy eta" [~1]
    forall f xs.
    concatMap @LBS.ByteString f xs = LBS.concatMap f xs
#-}


{-# inline [1] and #-}
-- and :: [Bool] -> Bool
and :: (Cons s s Bool Bool) => s -> Bool
and = foldr (&&) True
-- and = go
--   where
--     go a =
--       case uncons a of
--         Nothing -> True
--         Just (x, xs) -> x && go xs

{-# rules
"cons and/build"
    forall (g::forall b.(Bool->b->b)->b->b).
    and (build g) = g (&&) True

"cons and vector" [~1]
    and @(Vector Bool) = Data.Vector.and
"cons and vector eta" [~1]
    forall xs.
    and @(Vector Bool) xs = Data.Vector.and xs
#-}


{-# inline [1] or #-}
-- or :: [Bool] -> Bool
-- Foldable t => t Bool -> Bool
or :: (Cons s s Bool Bool) => s -> Bool
or = foldr (||) False
-- or = go
--   where
--     go a =
--       case uncons a of
--         Nothing -> False
--         Just (x, xs) -> x || go xs

{-# rules
"cons or/build"
    forall (g::forall b.(Bool->b->b)->b->b).
    or (build g) = g (||) False

"cons or vector" [~1]
    or @(Vector Bool) = Data.Vector.or
"cons or vector eta" [~1]
    forall xs.
    or @(Vector Bool) xs = Data.Vector.or xs
#-}


{-# noinline [1] any #-}
-- any :: (a -> Bool) -> [a] -> Bool
-- any :: Foldable t => (a -> Bool) -> t a -> Bool
any :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> Bool
-- any p = or . map p
any p = go
  where
  go s =
    case uncons s of
      Nothing -> False
      Just (x, xs) -> p x || go xs

{-# rules
"cons any/build"
    forall p (g::forall b.(a->b->b)->b->b).
    any p (build g) = g ((||) . p) False

"cons any text" [~1]
    any @Text = \p -> Data.Text.any p
"cons any text eta" [~1]
    forall p xs.
    any @Text p xs = Data.Text.any p xs

"cons any ltext" [~1]
    any @Data.Text.Lazy.Text = \p -> Data.Text.Lazy.any p
"cons any ltext eta" [~1]
    forall p xs.
    any @Data.Text.Lazy.Text p xs = Data.Text.Lazy.any p xs

"cons any vector"
    any @(Vector _) = Data.Vector.any
"cons any vector eta"
    forall p xs.
    any @(Vector _) p xs = Data.Vector.any p xs

"cons any bs"
    any @BS.ByteString = BS.any
"cons any bs eta"
    forall p xs.
    any @BS.ByteString p xs = BS.any p xs

"cons any bslazy"
    any @LBS.ByteString = LBS.any
"cons any bslazy eta"
    forall p xs.
    any @LBS.ByteString p xs = LBS.any p xs
#-}


{-# noinline [1] all #-}
-- all :: (a -> Bool) -> [a] -> Bool
-- all :: Foldable t => (a -> Bool) -> t a -> Bool
all :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> Bool
-- all p = and . map p
all p = go
  where
  go s =
    case uncons s of
      Nothing -> True
      Just (x, xs) -> p x && go xs

{-# rules
"cons all/build"
    forall p (g::forall b.(a->b->b)->b->b).
    all p (build g) = g ((&&) . p) True

"cons all text" [~1]
    all @Text = \p -> Data.Text.all p
"cons all text eta" [~1]
    forall p xs.
    all @Text p xs = Data.Text.all p xs

"cons all ltext" [~1]
    all @Data.Text.Lazy.Text = \p -> Data.Text.Lazy.all p
"cons all ltext eta" [~1]
    forall p xs.
    all @Data.Text.Lazy.Text p xs = Data.Text.Lazy.all p xs

"cons all vector" [~1]
    all @(Vector _) = Data.Vector.all
"cons all vector eta" [~1]
    forall p xs.
    all @(Vector _) p xs = Data.Vector.all p xs

"cons all bs" [~1]
    all @BS.ByteString = BS.all
"cons all bs eta" [~1]
    forall p xs.
    all @BS.ByteString p xs = BS.all p xs

"cons all bslazy" [~1]
    all @LBS.ByteString = LBS.all
"cons all bslazy eta" [~1]
    forall p xs.
    all @LBS.ByteString p xs = LBS.all p xs
#-}


{-# inline [2] sum #-}
-- sum :: (Num a) => [a] -> a
sum :: (Num a,  AsEmpty s, Cons s s a a) => s -> a
-- sum =  foldl (+) 0
sum = go
  where
  go s =
    case uncons s of
      Nothing -> 0
      Just (x, xs) -> x + go xs

{-# rules
"cons sum vector" [~1]
    sum @(Vector _) = Data.Vector.sum
"cons sum vector eta" [~1]
    forall xs.
    sum @(Vector _) xs = Data.Vector.sum xs

-- "cons sum seq" [~1]
--     sum @(Seq _) = Data.Foldable.sum
-- "cons sum seq eta" [~1]
--     forall xs.
--     sum @(Seq _) xs = Data.Foldable.sum xs
#-}


{-# inline [2] product #-}
-- product :: (Num a) => [a] -> a
product :: (Num a,  AsEmpty s, Cons s s a a) => s -> a
-- product =  foldl (*) 1
product = go
  where
  go s =
    case uncons s of
      Nothing -> 1
      Just (x, xs) -> x * go xs

{-# rules
"cons product vector" [~1]
    product @(Vector _) = Data.Vector.product
"cons product vector eta" [~1]
    forall xs.
    product @(Vector _) xs = Data.Vector.product xs

-- "cons product seq" [~1]
--     product @(Seq _) = Data.Foldable.product
-- "cons product seq eta" [~1]
--     forall xs.
--     product @(Seq _) xs = Data.Foldable.product xs
#-}


{-# inline [2] maximum #-}
-- maximum :: (Ord a) => [a] -> a
maximum :: (Ord a, Cons s s a a) => s -> a
maximum = go
  where
    go a =
      case uncons a of
        Nothing -> errorEmptyList "maximum"
        Just _ -> foldl1 max a

{-# rules
"cons maximum text" [~1]
    maximum @Char = Data.Text.maximum
"cons maximum text eta" [~1]
    forall xs.
    maximum @Char xs = Data.Text.maximum xs

"cons maximum ltext" [~1]
    maximum @Char = Data.Text.Lazy.maximum
"cons maximum ltext eta" [~1]
    forall xs.
    maximum @Char xs = Data.Text.Lazy.maximum xs

-- "cons maximum vector" [~1]
--     maximum @(Vector _) = Data.Vector.maximum
-- "cons maximum vector eta" [~1]
--     forall xs.
--     maximum @(Vector _) xs = Data.Vector.maximum xs

"cons maximum bs" [~1]
    maximum @Word8 = BS.maximum
"cons maximum bs eta" [~1]
    forall xs.
    maximum @Word8 xs = BS.maximum xs

"cons maximum bslazy" [~1]
    maximum @Word8 = LBS.maximum
"cons maximum bslazy eta" [~1]
    forall xs.
    maximum @Word8 xs = LBS.maximum xs

-- "cons maximum seq" [~1]
--     maximum @(Seq _) = Data.Foldable.maximum
-- "cons maximum seq eta" [~1]
--     forall xs.
--     maximum @(Seq _) xs = Data.Foldable.maximum xs
#-}


{-# inline [2] minimum #-}
-- minimum :: (Ord a) => [a] -> a
minimum :: (Ord a, Cons s s a a) => s -> a
minimum = go
  where
    go a =
      case uncons a of
        Nothing -> errorEmptyList "minimum"
        Just _ -> foldl1 min a

{-# rules
"cons minimum text" [~1]
    minimum @Char = Data.Text.minimum
"cons minimum text eta" [~1]
    forall xs.
    minimum @Char xs = Data.Text.minimum xs

"cons minimum ltext" [~1]
    minimum @Char = Data.Text.Lazy.minimum
"cons minimum ltext eta" [~1]
    forall xs.
    minimum @Char xs = Data.Text.Lazy.minimum xs

-- "cons minimum vector" [~1]
--     minimum @(Vector _) = Data.Vector.minimum
-- "cons minimum vector eta" [~1]
--     forall xs.
--     minimum @(Vector _) xs = Data.Vector.minimum xs

"cons minimum bs" [~1]
    minimum @Word8 = BS.minimum
"cons minimum bs eta" [~1]
    forall xs.
    minimum @Word8 xs = BS.minimum xs

"cons minimum bslazy" [~1]
    minimum @Word8 = LBS.minimum
"cons minimum bslazy eta" [~1]
    forall xs.
    minimum @Word8 xs = LBS.minimum xs

-- "cons minimum seq" [~1]
--     minimum @(Seq _) = Data.Foldable.minimum
-- "cons minimum seq eta" [~1]
--     forall xs.
--     minimum @(Seq _) xs = Data.Foldable.minimum xs
#-}
