{-
== Basic functions ==
+ (++)
+ head
+ last
+ tail
+ init
? uncons
+ null
+ length
-}

{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
-- {-# language PatternSynonyms #-}
-- {-# language RankNTypes #-}
-- {-# language ScopedTypeVariables #-}
module Consy.Basic
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  --, (++)
  , append
  , head
  , last
  , tail
  , init
  -- , uncons
  , null
  , length
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..))
import Data.Int (Int)
import Data.Function ((.), id)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.List (errorEmptyList)
import GHC.Num ((+))
import GHC.Real (Integral, fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds (augment, build, foldr)


{- ___ Basic functions ______________________________________________________ -}

{-# inline [1] append #-}
-- (++) :: [a] -> [a] -> [a]
append :: Cons s s a a => s -> s -> s
append = go
  where
    go a b =
      case uncons a of
        Nothing -> b
        Just (x, xs) -> x `cons` go xs b

{-# rules
"cons ++" [~1]
    forall xs ys.
    append xs ys = augment (\c n -> foldr c n xs) ys
"cons foldr/app" [1]
    forall ys.
    foldr (:) ys = \xs -> append xs ys

"cons append text" [~2]
    append @Text = Data.Text.append
"cons append text eta" [~2]
    forall a b.
    append @Text a b = Data.Text.append a b

"cons append ltext" [~2]
    append @Data.Text.Lazy.Text = Data.Text.Lazy.append
"cons append ltext eta" [~2]
    forall a b.
    append @Data.Text.Lazy.Text a b = Data.Text.Lazy.append a b

"cons append vector" [~2]
    append @(Vector _) = (Data.Vector.++)
"cons append vector eta" [~2]
    forall a b.
    append @(Vector _) a b = (Data.Vector.++) a b

"cons append bs" [~2]
    append @BS.ByteString = BS.append
"cons append bs eta" [~2]
    forall a b.
    append @BS.ByteString a b = BS.append a b

"cons append lbs" [~2]
    append @LBS.ByteString = LBS.append
"cons append lbs eta" [~2]
    forall a b.
    append @LBS.ByteString a b = LBS.append a b

"cons append seq" [~2]
    append @(Seq _) = (Data.Sequence.><)
"cons append seq eta" [~2]
    forall a b.
    append @(Seq _) a b = (Data.Sequence.><) a b
#-}

{-# noinline [1] head #-}
-- head :: [a] -> a
head :: (Cons s s a a) => s -> a
head s =
  case uncons s of
    Nothing -> badHead
    Just (x, _) -> x

badHead :: a
badHead = errorEmptyList "head"

{-# rules
"cons head/build"
    forall (g::forall b.(a->b->b)->b->b).
    head (build g) = g (\x _ -> x) (errorEmptyList "head")
"cons head/augment"
    forall xs (g::forall b. (a->b->b) -> b -> b).
    head (augment g xs) = g (\x _ -> x) (head xs)

"cons head text" [~2]
    head @Text = Data.Text.head
"cons head text eta" [~2]
    forall a.
    head @Text a = Data.Text.head a

"cons head ltext" [~2]
    head @Data.Text.Lazy.Text = Data.Text.Lazy.head
"cons head ltext eta" [~2]
    forall a.
    head @Data.Text.Lazy.Text a = Data.Text.Lazy.head a

"cons head vector" [~2]
    head @(Vector _) = Data.Vector.head
"cons head vector eta" [~2]
    forall a.
    head @(Vector _) a = Data.Vector.head a

"cons head bs" [~2]
    head @BS.ByteString = BS.head
"cons head bs eta" [~2]
    forall a.
    head @BS.ByteString a = BS.head a

"cons head lbs" [~2]
    head @LBS.ByteString = LBS.head
"cons head lbs eta" [~2]
    forall a.
    head @LBS.ByteString a = LBS.head a
#-}


{-# inline [1] last #-}
-- last :: [a] -> a
last :: (AsEmpty s, Cons s s a a) => s -> a
last = go
 where
   go s =
     case uncons s of
       Nothing -> errorEmptyList "tail"
       Just (x, Empty) -> x
       Just (_, xs) -> go xs

{-# rules
-- QUESTION: not sure about 'cons last' rule
-- "cons last" [~1]
--     forall xs.
--     last xs = foldr (\_ x -> x) (errorEmptyList "tail")  xs
-- "cons lastList" [1]
--     foldr (\_ x -> x) (errorEmptyList "tail") = last

"cons last text" [~2]
   last @Text = Data.Text.last
"cons last text eta" [~2]
   forall a.
   -- last @Data.Text.Text a = Data.Text.last a
   last @Data.Text.Text a = Data.Text.foldl (\_ x -> x) (errorEmptyList "tail") a

"cons last ltext" [~2]
   last @Data.Text.Lazy.Text = Data.Text.Lazy.last
"cons last ltext eta" [~2]
   forall a.
   last @Data.Text.Lazy.Text a = Data.Text.Lazy.last a

"cons last vector" [~2]
   last @(Vector _) = Data.Vector.last
"cons last vector eta" [~2]
   forall a.
   last @(Vector _) a = Data.Vector.last a

"cons last bs" [~2]
   last @BS.ByteString = BS.last
"cons last bs eta" [~2]
   forall a.
   last @BS.ByteString a = BS.last a

"cons last lbs" [~2]
   last @LBS.ByteString = LBS.last
"cons last lbs eta" [~2]
   forall a.
   last @LBS.ByteString a = LBS.last a
#-}


{-# inline [1] tail #-}
-- tail :: [a] -> [a]
tail :: (Cons s s a a) => s -> s
tail s =
  case uncons s of
    Nothing -> errorEmptyList "tail"
    Just (_, xs) -> xs

{-# rules
"cons tail text" [~2]
    tail @Text = Data.Text.tail
"cons tail text eta" [~2]
    forall a.
    tail @Data.Text.Text a = Data.Text.tail a

"cons tail ltext" [~2]
    tail @Data.Text.Lazy.Text = Data.Text.Lazy.tail
"cons tail ltext eta" [~2]
    forall a.
    tail @Data.Text.Lazy.Text a = Data.Text.Lazy.tail a

"cons tail vector" [~2]
    tail @(Vector _) = Data.Vector.tail
"cons tail vector eta" [~2]
    forall a.
    tail @(Vector _) a = Data.Vector.tail a

"cons tail bs" [~2]
    tail @BS.ByteString = BS.tail
"cons tail bs eta" [~2]
    forall a.
    tail @BS.ByteString a = BS.tail a

"cons tail lbs" [~2]
    tail @LBS.ByteString = LBS.tail
"cons tail lbs eta" [~2]
    forall a.
    tail @LBS.ByteString a = LBS.tail a
#-}


{-# inline [2] init #-}
-- init :: [a] -> [a]
init :: (AsEmpty s, Cons s s a a) => s -> s
init = go
  where
    go s =
      case uncons s of
        Nothing -> errorEmptyList "init"
        Just (x, xs) -> init' x xs
          where
            init' _ Empty = Empty
            init' y ys = case uncons ys of
                          Nothing -> y `cons` Empty
                          Just (z, zs) -> y `cons` (init' z zs)

{-# rules
"cons init text" [~2]
    init @Text = Data.Text.init
"cons init text eta" [~2]
    forall a.
    init @Data.Text.Text a = Data.Text.init a

"cons init ltext" [~2]
    init @Data.Text.Lazy.Text = Data.Text.Lazy.init
"cons init ltext eta" [~2]
    forall a.
    init @Data.Text.Lazy.Text a = Data.Text.Lazy.init a

"cons init vector" [~2]
    init @(Vector _) = Data.Vector.init
"cons init vector eta" [~2]
    forall a.
    init @(Vector _) a = Data.Vector.init a

"cons init bs" [~2]
    init @BS.ByteString = BS.init
"cons init bs eta" [~2]
    forall a.
    init @BS.ByteString a = BS.init a

"cons init lbs" [~2]
    init @LBS.ByteString = LBS.init
"cons init lbs eta" [~2]
    forall a.
    init @LBS.ByteString a = LBS.init a
#-}


-- -- QUESTION:  already implemented for Cons, does it required to reimplement?
-- {-# inline [2] uncons #-}
-- -- uncons :: [a] -> Maybe(a, [a])
-- uncons :: (AsEmpty s, Cons s s a a) => s -> Maybe (a, s)
-- uncons = \s ->
--       case uncons s of
--         Nothing -> Nothing
--         Just (a,s) -> Just (a,s)
--
-- {-# rules
-- "cons uncons text" [~2]
--     Consy.uncons @Text = Data.Text.uncons
-- "cons uncons text eta" [~2]
--     forall a.
--     Consy.uncons @Data.Text.Text a = Data.Text.uncons a
--
-- "cons uncons ltext" [~2]
--     Consy.uncons @Data.Text.Lazy.Text = Data.Text.Lazy.uncons
-- "cons uncons ltext eta" [~2]
--     forall a.
--     Consy.uncons @Data.Text.Lazy.Text a = Data.Text.Lazy.uncons a
--
-- "cons uncons bs" [~2]
--     uncons @BS.ByteString = BS.uncons
-- "cons uncons bs eta" [~2]
--     forall a.
--     uncons @BS.ByteString a = BS.uncons a
--
-- "cons uncons lbs" [~2]
--     uncons @LBS.ByteString = LBS.uncons
-- "cons uncons lbs eta" [~2]
--     forall a.
--     uncons @LBS.ByteString a = LBS.uncons a
-- #-}


{-# inline [2] null #-}
-- null :: [a] -> Bool
null :: (AsEmpty s, Cons s s a a) => s -> Bool
null = \s ->
      case uncons s of
        Nothing -> True
        _ -> False

{-# rules
"cons null text" [~2]
    null @Text = Data.Text.null
"cons null text eta" [~2]
    forall a.
    null @Data.Text.Text a = Data.Text.null a

"cons null ltext" [~2]
    null @Data.Text.Lazy.Text = Data.Text.Lazy.null
"cons null ltext eta" [~2]
    forall a.
    null @Data.Text.Lazy.Text a = Data.Text.Lazy.null a

"cons null vector" [~2]
    null @(Vector _) = Data.Vector.null
"cons null vector eta" [~2]
    forall a.
    null @(Vector _) a = Data.Vector.null a

"cons null bs" [~2]
    null @BS.ByteString = BS.null
"cons null bs eta" [~2]
    forall a.
    null @BS.ByteString a = BS.null a

"cons null lbs" [~2]
    null @LBS.ByteString = LBS.null
"cons null lbs eta" [~2]
    forall a.
    null @LBS.ByteString a = LBS.null a

"cons null seq" [~2]
    null @(Seq _) = Data.Sequence.null
"cons null seq eta" [~2]
    forall a.
    null @(Seq _) a = Data.Sequence.null a
#-}


{-# inline [0] length #-}
-- length :: [a] -> Int
length :: (Cons s s a a, Integral n) => s -> n
length = go 0
  where
    go !n s =
      case uncons s of
        Nothing -> n
        Just (_, xs) -> go (n+1) xs

-- lenACC :: [a] -> Int -> Int
lenAcc :: (Cons s s a a, Integral n) => n -> s -> n
lenAcc !n s =
  case uncons s of
    Nothing -> n
    Just (_, xs) -> lenAcc (n+1) xs

{-# rules
"cons length" [~1]
    forall xs.
    length xs = foldr lengthFB idLength xs 0
"cons lengthList" [1]
    foldr lengthFB idLength = lenAcc

"cons length text"
    length @Text = fromIntegral . Data.Text.length
"cons length text eta"
    forall xs.
    length @Text xs = fromIntegral (Data.Text.length xs)

"cons length ltext"
    length @Data.Text.Lazy.Text = fromIntegral . Data.Text.Lazy.length
"cons length ltext eta"
    forall xs.
    length @Data.Text.Lazy.Text xs = fromIntegral (Data.Text.Lazy.length xs)

"cons length vector"
    length @(Vector _) = fromIntegral . Data.Vector.length
"cons length vector eta"
    forall xs.
    length @(Vector _) xs = fromIntegral (Data.Vector.length xs)

"cons length bs"
    length @BS.ByteString = BS.length
"cons length bs eta"
    forall xs.
    length @BS.ByteString xs = fromIntegral (BS.length xs)

"cons length bslazy"
    length @LBS.ByteString = LBS.length
"cons length bslazy eta"
    forall xs.
    length @LBS.ByteString xs = fromIntegral (LBS.length xs)

"cons length seq"
    length @(Seq _) = Data.Sequence.length
"cons length seq eta"
    forall xs.
    length @(Seq _) xs = fromIntegral (Data.Sequence.length xs)
 #-}

{-# inline [0] lengthFB #-}
-- lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r = \ !a -> r (a + 1)

{-# inline [0] idLength #-}
-- idLength :: Int -> Int
idLength :: Int -> Int
idLength = id
