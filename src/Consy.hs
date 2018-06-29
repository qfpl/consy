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
  , foldr2
  , build
  , length
  , foldl'
  , filter
  , map
  , repeat
  , take
  , replicate
  , zipWith
  , append
  , stripSuffix
  , stripPrefix
  , prefixed
  , suffixed
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Control.Lens.Prism (Prism', prism')
import Control.Monad (guard)
import Data.Bool ((&&), Bool(..), otherwise)
import Data.Char (Char)
import Data.Int (Int)
import Data.Eq (Eq(..))
import Data.Function ((.), id, const)
import Data.Functor ((<$))
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Base (oneShot, seq)
import GHC.Num ((+), (-))
import GHC.Real (Integral, fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

{-# inline [1] augment #-}
augment :: Cons s s a a => (forall b. (a->b->b) -> b -> b) -> s -> s
augment g xs = g cons xs
{-# rules
"cons foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b).
  foldr k z (augment g xs) = g k (foldr k z xs)
#-}

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
"cons foldr/build" [2] forall f z (g :: forall b. (a -> b -> b) -> b -> b).
  foldr f z (build g) = g f z

"cons foldr text" [~2] foldr @Data.Text.Text = Data.Text.foldr
"cons foldr text eta" [~2] forall f z xs.
  foldr @Data.Text.Text f z xs = Data.Text.foldr f z xs

"cons foldr vector" [~2] foldr @(Vector _) = Data.Vector.foldr
"cons foldr vector eta" [~2] forall f z xs.
  foldr @(Vector _) f z xs = Data.Vector.foldr f z xs

"cons foldr ltext" [~2] foldr @Data.Text.Lazy.Text = Data.Text.Lazy.foldr
"cons foldr ltext eta" [~2] forall f z xs.
  foldr @Data.Text.Lazy.Text f z xs = Data.Text.Lazy.foldr f z xs

"cons foldr bs" [~2] foldr @BS.ByteString = BS.foldr
"cons foldr bs eta" [~2] forall f z xs. foldr @BS.ByteString f z xs = BS.foldr f z xs

"cons foldr lbs" [~2] foldr @LBS.ByteString = LBS.foldr
"cons foldr lbs eta" [~2] forall f z xs. foldr @LBS.ByteString f z xs = LBS.foldr f z xs

"cons foldr seq" [~2] foldr @(Seq _) = Data.Foldable.foldr
"cons foldr seq eta" [~2] forall f z xs. foldr @(Seq _) f z xs = Data.Foldable.foldr f z xs
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

"cons foldl' vector" [~2] foldl' @(Vector _) = Data.Vector.foldl'
"cons foldl' vector eta" [~2] forall f z xs.
  foldl' @(Vector _) f z xs = Data.Vector.foldl' f z xs

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

"cons length ltext" length @Data.Text.Lazy.Text =
  fromIntegral . Data.Text.Lazy.length
"cons length ltext eta" forall xs.
  length @Data.Text.Lazy.Text xs = fromIntegral (Data.Text.Lazy.length xs)

"cons length vector" length @(Vector _) = fromIntegral . Data.Vector.length
"cons length vector eta" forall xs.
  length @(Vector _) xs = fromIntegral (Data.Vector.length xs)

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

"cons map ltext" map @Data.Text.Lazy.Text = Data.Text.Lazy.map
"cons map ltext eta" forall f xs. map @Data.Text.Lazy.Text f xs = Data.Text.Lazy.map f xs

"cons map text" map @Text = Data.Text.map
"cons map text eta" forall f xs. map @Text f xs = Data.Text.map f xs

"cons map vector" map @(Vector _) = Data.Vector.map
"cons map vector eta" forall f xs. map @(Vector _) f xs = Data.Vector.map f xs

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

"cons filter ltext" filter @Data.Text.Lazy.Text @Char = Data.Text.Lazy.filter
"cons filter ltext eta" forall p xs.
  filter @Data.Text.Lazy.Text @Char p xs = Data.Text.Lazy.filter p xs

"cons filter text" filter @Text @Char = Data.Text.filter
"cons filter text eta" forall p xs. filter @Text @Char p xs = Data.Text.filter p xs

"cons filter vector" filter @(Vector _) = Data.Vector.filter
"cons filter vector eta" forall p xs. filter @(Vector _) p xs = Data.Vector.filter p xs

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

"cons repeat ltext" repeat @Data.Text.Lazy.Text = Data.Text.Lazy.repeat

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

"cons take ltext" take @Data.Text.Lazy.Text = \n -> Data.Text.Lazy.take (fromIntegral n)
"cons take ltext eta" forall n xs.
  take @Data.Text.Lazy.Text n xs = Data.Text.Lazy.take (fromIntegral n) xs

"cons take vector" take @(Vector _) = Data.Vector.take
"cons take vector eta" forall n xs. take @(Vector _) n xs = Data.Vector.take n xs

"cons take bs" take @BS.ByteString = BS.take . fromIntegral
"cons take bs eta" forall n xs. take @BS.ByteString n xs = BS.take (fromIntegral n) xs

"cons take bslazy" take @LBS.ByteString = LBS.take . fromIntegral
"cons take bslazy eta" forall n xs. take @LBS.ByteString n xs = LBS.take (fromIntegral n) xs

"cons take seq" take @(Seq _) = Data.Sequence.take
"cons take seq eta" forall n xs. take @(Seq _) n xs = Data.Sequence.take n xs
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

"cons replicate ltext" [~2]
  replicate @Data.Text.Lazy.Text @Char =
    \n x -> Data.Text.Lazy.replicate (fromIntegral n) (Data.Text.Lazy.singleton x)
"cons replicate ltext eta" [~2] forall n x.
  replicate @Data.Text.Lazy.Text @Char n x =
    Data.Text.Lazy.replicate (fromIntegral n) (Data.Text.Lazy.singleton x)

"cons replicate vector" [~2] replicate @(Vector _) = Data.Vector.replicate
"cons replicate vector eta" [~2] forall n x.
  replicate @(Vector _) n x = Data.Vector.replicate n x

"cons replicate bs" [~2]
  replicate @BS.ByteString = \n x -> BS.replicate (fromIntegral n) x
"cons replicate bs eta" [~2] forall n x.
  replicate @BS.ByteString n x = BS.replicate (fromIntegral n) x

"cons replicate bslazy" [~2]
  replicate @LBS.ByteString = \n x -> LBS.replicate (fromIntegral n) x
"cons replicate bslazy eta" [~2] forall n x.
  replicate @LBS.ByteString n x = LBS.replicate (fromIntegral n) x
#-}

{-# inline [1] append #-}
append :: Cons s s a a => s -> s -> s
append = go
  where
    go a b =
      case uncons a of
        Nothing -> b
        Just (x, xs) -> x `cons` go xs b
{-# rules
"cons ++" [~1] forall xs ys. append xs ys = augment (\c n -> foldr c n xs) ys
"cons foldr/app" [1] forall ys. foldr (:) ys = \xs -> append xs ys

"cons append text" [~2] append @Text = Data.Text.append
"cons append text eta" [~2] forall a b. append @Text a b = Data.Text.append a b

"cons append ltext" [~2] append @Data.Text.Lazy.Text = Data.Text.Lazy.append
"cons append ltext eta" [~2] forall a b.
  append @Data.Text.Lazy.Text a b = Data.Text.Lazy.append a b

"cons append vector" [~2] append @(Vector _) = (Data.Vector.++)
"cons append vector eta" [~2] forall a b. append @(Vector _) a b = (Data.Vector.++) a b

"cons append bs" [~2] append @BS.ByteString = BS.append
"cons append bs eta" [~2] forall a b. append @BS.ByteString a b = BS.append a b

"cons append lbs" [~2] append @LBS.ByteString = LBS.append
"cons append lbs eta" [~2] forall a b. append @LBS.ByteString a b = LBS.append a b

"cons append seq" [~2] append @(Seq _) = (Data.Sequence.><)
"cons append seq eta" [~2] forall a b. append @(Seq _) a b = (Data.Sequence.><) a b
#-}

prefixed :: (Cons s s a a, Eq a) => s -> Prism' s s
prefixed ps = prism' (ps `append`) (stripPrefix ps)
{-# inline prefixed #-}

suffixed :: (AsEmpty s, Eq s, Cons s s a a, Eq a) => s -> Prism' s s
suffixed qs = prism' (`append` qs) (stripSuffix qs)
{-# inline suffixed #-}

stripPrefix :: (Cons s s a a, Eq a) => s -> s -> Maybe s
stripPrefix s ys =
  case uncons s of
    Nothing -> Just ys
    Just (x, xs)
      | Just (y', ys') <- uncons ys
      ,  x == y' -> stripPrefix xs ys'
    _ -> Nothing
{-# inline stripPrefix #-}

foldr2 :: (Cons s s a a, Cons t t b b) => (a -> b -> c -> c) -> c -> s -> t -> c
foldr2 k z = go
  where
    go xs ys =
      case uncons xs of
        Nothing -> z
        Just (x', xs') ->
          case uncons ys of
            Nothing -> z
            Just (y', ys') -> k x' y' (go xs' ys')
{-# inline [0] foldr2 #-}

foldr2_left :: Cons s s b b => (a -> b -> c -> d) -> d -> a -> (s -> c) -> s -> d
foldr2_left k z x r y =
  case uncons y of
    Nothing -> z
    Just (y', ys') -> k x y' (r ys')

{-# rules
"cons foldr2/left" forall k z ys (g::forall b.(a->b->b)->b->b).
  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
#-}

zipWith
  :: ( Cons s s a a
     , Cons t t b b
     , AsEmpty u, Cons u u c c
     )
  => (a -> b -> c) -> s -> t -> u
zipWith f = go
  where
    go s t =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) ->
          case uncons t of
            Nothing -> Empty
            Just (y, ys) -> f x y `cons` go xs ys
{-# noinline [1] zipWith #-}

{-# inline [0] zipWithFB #-}
zipWithFB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB c f = \x y r -> (x `f` y) `c` r

{-# rules
"cons zipWith" [~1] forall f xs ys.
  zipWith f xs ys = build (\c n -> foldr2 (zipWithFB c f) n xs ys)
"cons zipWithList" [1] forall f. foldr2 (zipWithFB (:) f) [] = zipWith f

"cons zipWith text" [~2]
  zipWith @Data.Text.Text @_ @Data.Text.Text @_ @Data.Text.Text @_ =
    Data.Text.zipWith
"cons zipWith text eta" [~2]
  forall f a b.
  zipWith @Data.Text.Text @_ @Data.Text.Text @_ @Data.Text.Text @_ f a b =
    Data.Text.zipWith f a b

"cons zipWith ltext" [~2]
  zipWith @Data.Text.Lazy.Text @_ @Data.Text.Lazy.Text @_ @Data.Text.Lazy.Text @_ =
    Data.Text.Lazy.zipWith
"cons zipWith ltext eta" [~2]
  forall f a b.
  zipWith @Data.Text.Lazy.Text @_ @Data.Text.Lazy.Text @_ @Data.Text.Lazy.Text @_ f a b =
    Data.Text.Lazy.zipWith f a b

"cons zipWith bs" [~2]
  zipWith @BS.ByteString @_ @BS.ByteString @_ @[_] @_ =
    BS.zipWith
"cons zipWith bs eta" [~2]
  forall f a b.
  zipWith @BS.ByteString @_ @BS.ByteString @_ @[_] @_ f a b =
    BS.zipWith f a b

"cons zipWith lbs" [~2]
  zipWith @LBS.ByteString @_ @LBS.ByteString @_ @[_] @_ =
    LBS.zipWith
"cons zipWith lbs eta" [~2]
  forall f a b.
  zipWith @LBS.ByteString @_ @LBS.ByteString @_ @[_] @_ f a b =
    LBS.zipWith f a b

"cons zipWith seq" [~2]
  zipWith @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ =
    Data.Sequence.zipWith
"cons zipWith seq eta" [~2]
  forall f a b.
  zipWith @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ f a b =
    Data.Sequence.zipWith f a b
#-}

stripSuffix :: (AsEmpty s, Eq s, Cons s s a a, Eq a) => s -> s -> Maybe s
stripSuffix qs xs0 = go xs0 zs
  where
    zs = drp qs xs0

    drp a b =
      case uncons a of
        Just (_, ps) ->
          case uncons b of
            Just (_, xs) -> drp ps xs
            Nothing -> Empty
        Nothing -> b

    go a b =
      case uncons b of
        Just (_, ys) ->
          case uncons a of
            Just (_, xs) -> go xs ys
            Nothing -> Nothing
        Nothing -> zipWith const xs0 zs <$ guard (a == qs)
{-# inline stripSuffix #-}
