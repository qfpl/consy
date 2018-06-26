{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms, ViewPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
module Consy (foldr, length, foldl', filter, map, repeat, take, replicate, build) where

import Control.Lens.Cons (Cons, uncons)
import qualified Control.Lens.Cons
import Control.Lens.Empty (AsEmpty, pattern Empty)
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

import qualified Data.Foldable
import qualified Data.Sequence
import qualified Data.Text

{-# inline [0] cons #-}
cons :: Cons s s a a => a -> s -> s
cons = Control.Lens.Cons.cons

{-# inline [0] consText #-}
consText :: Char -> Text -> Text
consText = Data.Text.cons

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

-- consy foldr is actually faster for Text
-- "cons foldr text" foldr @Text @Char = Data.Text.foldr
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
#-}

{-# noinline [1] length #-}
length :: Cons s s a a => s -> Int
length = go 0
  where
    go !n s =
      case uncons s of
        Nothing -> n
        Just (_, xs) -> go (n+1) xs

lenAcc :: Cons s s a a => Int -> s -> Int
lenAcc !n s =
  case uncons s of
    Nothing -> n
    Just (_, xs) -> lenAcc (n+1) xs

{-# rules
"cons length" [~1] forall xs . length xs = foldr lengthFB idLength xs 0
"cons lengthList" [1] foldr lengthFB idLength = lenAcc

"cons length text" length @Text = Data.Text.length
"cons length text eta" forall xs. length @Text xs = Data.Text.length xs
 #-}

{-# inline [0] lengthFB #-}
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r = \ !a -> r (a + 1)

{-# inline [0] idLength #-}
idLength :: Int -> Int
idLength = id

{-# noinline [0] map #-}
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
"cons mapList" [1]  forall f. foldr (mapFB cons f) [] = map f
"cons mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f.g)
"cons mapFB/id" forall c. mapFB c (\x -> x) = c

"cons map text" map @Text = Data.Text.map
"cons map text eta" forall f xs. map @Text f xs = Data.Text.map f xs
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
"cons filterList" [1] forall p. foldr (filterFB cons p) [] = filter p
"cons filterFB" forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)

"cons filter text" filter @Text @Char = Data.Text.filter
"cons filter text eta" forall p xs. filter @Text @Char p xs = Data.Text.filter p xs

"cons filter seq" filter @(Seq _) = Data.Sequence.filter
"cons filter seq eta" forall p xs. filter @(Seq _) p xs = Data.Sequence.filter p xs
#-}

repeat :: (AsEmpty s, Cons s s a a) => a -> s
{-# inline [0] repeat #-}
repeat x = xs where xs = x `cons` xs

{-# inline [0] repeatFB #-}
repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = x `c` xs

{-# rules
"cons repeat" [~1] forall x. repeat x = build (\c _ -> repeatFB c x)
"cons repeatFB list" [1] repeatFB (:) = repeat
"cons repeatFB text" [1] repeatFB consText = repeat
#-}

{-# inline [1] take #-}
take :: (AsEmpty s, Cons s s a a) => Int -> s -> s
take n xs
  | 0 < n = unsafeTake n xs
  | otherwise = Empty

{-# noinline [1] unsafeTake #-}
unsafeTake :: (AsEmpty s, Cons s s a a) => Int -> s -> s
unsafeTake !_ (uncons -> Nothing) = Empty
unsafeTake 1 (uncons -> Just (x, _)) = x `cons` Empty
unsafeTake m (uncons -> Just (x, xs)) = x `cons` unsafeTake (m - 1) xs

{-# rules
"cons take" [~1] forall n xs.
  take n xs =
  build (\c nil -> if 0 < n
                   then foldr (takeFB c nil) (flipSeqTake nil) xs n
                   else nil)
"cons unsafeTakeList" [1] forall n xs.
  foldr (takeFB cons []) (flipSeqTake []) xs n = unsafeTake n xs

"cons take text" take @Text = Data.Text.take
"cons take text eta" forall n xs. take @Text n xs = Data.Text.take n xs
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
replicate n x = take n (repeat x)

{-# rules
"cons replicate text" [~2] forall n x.
  replicate @Text @Char n x = Data.Text.replicate n (Data.Text.singleton x)
#-}
