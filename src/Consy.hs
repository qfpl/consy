{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
module Consy
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , foldr
  , length
  , foldl'
  , filter
  , map
  , repeat
  , take
  , replicate
  , build
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool ((&&), Bool(..), otherwise)
import Data.Char (Char)
import Data.Int (Int)
import Data.Function ((.), id)
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Base (oneShot, seq)
import GHC.Num ((+), (-))
import GHC.Real (Integral, fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text

{-# inline [1] build #-}
build
  :: (AsEmpty s, Cons s s a a)
  => (forall b. (a -> b -> b) -> b -> b)
  -> s
build g = g cons Empty

{-# inline [0] foldr #-}
foldr :: Cons s s a a => (a -> b -> b) -> b -> s -> b
foldr f z = go
  where
    go s =
      case uncons s of
        Nothing -> z
        Just (x, xs) -> f x (go xs)

{-# rules
"cons foldr/build" forall f z (g :: forall b. (a -> b -> b) -> b -> b).
  foldr f z (build g) = g f z

-- consy foldr is faster than Text's
-- consy foldr is faster than ByteString's

"cons foldr lbs" foldr @LBS.ByteString = LBS.foldr
"cons foldr lbs eta" forall f z xs. foldr @LBS.ByteString f z xs = LBS.foldr f z xs

"cons foldr seq" foldr @(Seq _) = Data.Foldable.foldr
"cons foldr seq eta" forall f z xs. foldr @(Seq _) f z xs = Data.Foldable.foldr f z xs
#-}

{-# inline [2] foldl' #-}
foldl' :: forall s a b. Cons s s a a => (b -> a -> b) -> b -> s -> b
foldl' k z0 xs =
  foldr
    (\(v :: a) (fn :: b -> b) -> oneShot (\(z :: b) -> z `seq` fn (k z v)))
    (id :: b -> b)
    xs
    z0
{-# rules
"cons foldl' text" [~2] foldl' @Text @Char = Data.Text.foldl'
"cons foldl' text eta" [~2] forall f z xs.
  foldl' @Text @Char f z xs = Data.Text.foldl' f z xs

"cons foldl' seq" [~2] foldl' @(Seq _) = Data.Foldable.foldl'
"cons foldl' seq eta" [~2] forall f z xs.
  foldl' @(Seq _) f z xs = Data.Foldable.foldl' f z xs

"cons foldl' bs" [~2] foldl' @BS.ByteString = BS.foldl'
"cons foldl' bs eta" [~2] forall f z xs.
  foldl' @BS.ByteString f z xs = BS.foldl' f z xs

"cons foldl' bslazy" [~2] foldl' @LBS.ByteString = LBS.foldl'
"cons foldl' bslazy eta" [~2] forall f z xs.
  foldl' @LBS.ByteString f z xs = LBS.foldl' f z xs
#-}

{-# inline [0] length #-}
length :: (Cons s s a a, Integral n) => s -> n
length = go 0
  where
    go !n s =
      case uncons s of
        Nothing -> n
        Just (_, xs) -> go (n+1) xs

lenAcc :: (Cons s s a a, Integral n) => n -> s -> n
lenAcc !n s =
  case uncons s of
    Nothing -> n
    Just (_, xs) -> lenAcc (n+1) xs

{-# rules
"cons length" [~1] forall xs . length xs = foldr lengthFB idLength xs 0
"cons lengthList" [1] foldr lengthFB idLength = lenAcc

"cons length text" length @Text = fromIntegral . Data.Text.length
"cons length text eta" forall xs. length @Text xs = fromIntegral (Data.Text.length xs)

"cons length bs" length @BS.ByteString = BS.length
"cons length bs eta" forall xs. length @BS.ByteString xs = fromIntegral (BS.length xs)

"cons length bslazy" length @LBS.ByteString = LBS.length
"cons length bslazy eta" forall xs. length @LBS.ByteString xs = fromIntegral (LBS.length xs)

"cons length seq" length @(Seq _) = Data.Sequence.length
"cons length seq eta" forall xs. length @(Seq _) xs = fromIntegral (Data.Sequence.length xs)
 #-}

{-# inline [0] lengthFB #-}
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r = \ !a -> r (a + 1)

{-# inline [0] idLength #-}
idLength :: Int -> Int
idLength = id

{-# inline [0] map #-}
map :: (AsEmpty s, Cons s s a a) => (a -> a) -> s -> s
map f = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) -> f x `cons` go xs

{-# inline [0] mapFB #-}
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapFB c f = \x ys -> c (f x) ys

{-# rules
"cons map" [~1] forall f xs. map f xs = build (\c n -> foldr (mapFB c f) n xs)
"cons mapList list" [1]  forall f. foldr (mapFB (:) f) [] = map f

"cons mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f.g)
"cons mapFB/id" forall c. mapFB c (\x -> x) = c

"cons map text" map @Text = Data.Text.map
"cons map text eta" forall f xs. map @Text f xs = Data.Text.map f xs

"cons map bs" map @BS.ByteString = BS.map
"cons map bs eta" forall f xs. map @BS.ByteString f xs = BS.map f xs

"cons map bslazy" map @LBS.ByteString = LBS.map
"cons map bslazy eta" forall f xs. map @LBS.ByteString f xs = LBS.map f xs

"cons map seq" map @(Seq _) = Data.Functor.fmap
"cons map seq eta" forall f xs. map @(Seq _) f xs = Data.Functor.fmap f xs
#-}

{-# noinline [0] filter #-}
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
"cons filter" [~1] forall p xs. filter p xs = build (\c n -> foldr (filterFB c p) n xs)
"cons filterFB" forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)

"cons filterList" [1] forall p. foldr (filterFB (:) p) [] = filter p

"cons filter text" filter @Text @Char = Data.Text.filter
"cons filter text eta" forall p xs. filter @Text @Char p xs = Data.Text.filter p xs

"cons filter bs" filter @BS.ByteString = BS.filter
"cons filter bs eta" forall p xs. filter @BS.ByteString p xs = BS.filter p xs

"cons filter bslazy" filter @LBS.ByteString = LBS.filter
"cons filter bslazy eta" forall p xs. filter @LBS.ByteString p xs = LBS.filter p xs

"cons filter seq" filter @(Seq _) = Data.Sequence.filter
"cons filter seq eta" forall p xs. filter @(Seq _) p xs = Data.Sequence.filter p xs
#-}

repeat :: (AsEmpty s, Cons s s a a) => a -> s
{-# inline [0] repeat #-}
repeat x = xs where xs = x `cons` xs

{-# inline [0] repeatFB #-}
repeatFB :: (a -> b -> b) -> b -> a -> b
repeatFB c _ x = xs where xs = x `c` xs

{-# rules
"cons repeat" [~1] forall x. repeat x = build (\c n -> repeatFB c n x)
"cons repeatFB" [1] repeatFB (:) [] = repeat

"cons repeat bslazy" repeat @LBS.ByteString = LBS.repeat
#-}

{-# inline [1] take #-}
take :: (AsEmpty s, Cons s s a a) => Int -> s -> s
take = go
  where
    go !n s
      | 0 < n =
          case uncons s of
            Nothing -> Empty
            Just (x, xs) -> x `cons` go (n-1) xs
      | otherwise = Empty

{-# noinline [1] unsafeTake #-}
unsafeTake :: (AsEmpty s, Cons s s a a) => Int -> s -> s
unsafeTake !m s =
  case m of
    1 -> case uncons s of; Just (x, xs) -> x `cons` Empty
    _ ->
      case uncons s of
        Nothing -> Empty
        Just (x, xs) -> x `cons` unsafeTake (m - 1) xs

{-# rules
"cons take" [~1] forall n xs.
  take n xs =
  build (\c nil -> if 0 < n
                   then foldr (takeFB c nil) (flipSeqTake nil) xs n
                   else nil)

"cons unsafeTakeList" [1] forall n xs.
  foldr (takeFB (:) []) (flipSeqTake []) xs n =
    unsafeTake n xs

"cons take text" take @Text = Data.Text.take
"cons take text eta" forall n xs. take @Text n xs = Data.Text.take n xs

"cons take bs" take @BS.ByteString = BS.take . fromIntegral
"cons take bs eta" forall n xs. take @BS.ByteString n xs = BS.take (fromIntegral n) xs

"cons take bslazy" take @LBS.ByteString = LBS.take . fromIntegral
"cons take bslazy eta" forall n xs. take @LBS.ByteString n xs = LBS.take (fromIntegral n) xs
#-}

{-# inline [0] flipSeqTake #-}
flipSeqTake :: a -> Int -> a
flipSeqTake x !_ = x

{-# inline [0] takeFB #-}
takeFB :: (a -> b -> b) -> b -> a -> (Int -> b) -> Int -> b
takeFB c n x xs =
  \m -> case m of
    1 -> x `c` n
    _ -> x `c` xs (m - 1)

{-# inline [2] replicate #-}
replicate :: (AsEmpty s, Cons s s a a) => Int -> a -> s
replicate = \n x -> take n (repeat x)

{-# rules
"cons replicate text" [~2]
  replicate @Text @Char = \n x -> Data.Text.replicate n (Data.Text.singleton x)
"cons replicate text eta" [~2] forall n x.
  replicate @Text @Char n x = Data.Text.replicate n (Data.Text.singleton x)

"cons replicate bs" [~2]
  replicate @BS.ByteString = \n x -> BS.replicate (fromIntegral n) x
"cons replicate bs eta" [~2] forall n x.
  replicate @BS.ByteString n x = BS.replicate (fromIntegral n) x

"cons replicate bslazy" [~2]
  replicate @LBS.ByteString = \n x -> LBS.replicate (fromIntegral n) x
"cons replicate bslazy eta" [~2] forall n x.
  replicate @LBS.ByteString n x = LBS.replicate (fromIntegral n) x
#-}
