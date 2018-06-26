{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
module Consy (foldr, length, foldl', filter, map, build) where

import Control.Lens.Cons (Cons, uncons)
import qualified Control.Lens.Cons
import Control.Lens.Empty (AsEmpty, pattern Empty)
import Data.Bool ((&&), Bool(..), otherwise)
import Data.Int (Int)
import Data.Function ((.), id)
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Base (oneShot, seq)
import GHC.Num ((+))

{-# inline [0] cons #-}
cons :: Cons s s a a => a -> s -> s
cons = Control.Lens.Cons.cons

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
"cons foldr text" forall f z (xs :: Text). foldr f z xs = Data.Text.foldr f z xs
#-}

{-# inline foldl' #-}
foldl' :: forall s a b. Cons s s a a => (b -> a -> b) -> b -> s -> b
foldl' k z0 xs =
  foldr
    (\(v :: a) (fn :: b -> b) -> oneShot (\(z :: b) -> z `seq` fn (k z v)))
    (id :: b -> b)
    xs
    z0

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

{-# RULES
"cons length" [~1] forall xs . length xs = foldr lengthFB idLength xs 0
"cons lengthList" [1] foldr lengthFB idLength = lenAcc
"cons length text" forall (xs :: Text). length xs = Data.Text.length xs
 #-}

{-# INLINE [0] lengthFB #-}
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r = \ !a -> r (a + 1)

{-# INLINE [0] idLength #-}
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

{-# RULES
"cons map" [~1] forall f xs. map f xs = build (\c n -> foldr (mapFB c f) n xs)
"cons mapList" [1]  forall f. foldr (mapFB cons f) [] = map f
"cons mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f.g)
"cons mapFB/id" forall c. mapFB c (\x -> x) = c
"cons map text" forall f (xs :: Text). map f xs = Data.Text.map f xs
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

{-
Unfortunatly, rewrite rules don't work for partially applied functions.
So the filter in `map (filter f) :: [Text] -> [Text]` won't be re-written to Data.Text.filter.
-}
{-# RULES
"cons filter" [~1] forall p xs. filter p xs = build (\c n -> foldr (filterFB c p) n xs)
"cons filterList" [1] forall p. foldr (filterFB cons p) [] = filter p
"cons filterFB" forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
"cons filter text" forall f (xs :: Text). filter f xs = Data.Text.filter f xs
 #-}
