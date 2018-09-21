{-
== Zipping and unzipping Sublist ==
+ zip
+ zip3
+ zip4
+ zip5
+ zip6
+ zip7
+ zipWith
+ zipWith3
+ zipWith4
+ zipWith5
+ zipWith6
+ zipWith7
+ unzip
+ unzip3
+ unzip4
+ unzip5
+ unzip6
+ unzip7
-}

{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
-- {-# language BangPatterns #-}
-- {-# language PatternSynonyms #-}
-- {-# language RankNTypes #-}
-- {-# language ScopedTypeVariables #-}
module Consy.Zipping
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , zip7
  , zipWith
  , zipWith3
  , zipWith4
  , zipWith5
  , zipWith6
  , zipWith7
  , unzip
  , unzip3
  , unzip4
  , unzip5
  , unzip6
  , unzip7
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Control.Lens.Prism (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds


{- ___ Zipping and unzipping Sublist ________________________________________ -}

{-# inline [0] foldr2 #-}
-- foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
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

-- foldr2_left :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left :: Cons s s b b => (a -> b -> c -> d) -> d -> a -> (s -> c) -> s -> d
foldr2_left k z x r y =
  case uncons y of
    Nothing -> z
    Just (y', ys') -> k x y' (r ys')

{-# rules
"cons foldr2/left"
    forall k z ys (g::forall b.(a->b->b)->b->b).
    foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
#-}

{- QUESTION: Cons u u (a,b)(a,b)   or    Cons u u c c      -}
{-# inline [1] zip #-}
-- zip :: [a] -> [b] -> [(a,b)]
zip ::
    ( Cons s s a a
    , Cons t t b b
    , AsEmpty u,  Cons u u (a,b) (a,b)
    )
  => s -> t -> u
zip = go
  where
    go s t =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) -> ((,) a b) `cons` go as bs

{-# inline [0] zipFB #-}
zipFB :: ((a, b) -> c -> d) -> a -> b -> c -> d
zipFB c = \x y r -> (x,y) `c` r

{-# rules
"cons zip" [2]
    forall xs ys.
    zip xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
"cons zipList" [1]
    foldr2 (zipFB (:)) [] = zip

"cons zip text" [~2]
    zip @Data.Text.Text @_ @Data.Text.Text @_ = Data.Text.zip
"cons zip text eta" [~2]
    forall a b.
    zip @Data.Text.Text @_ @Data.Text.Text @_ a b = Data.Text.zip a b

"cons zip ltext" [~2]
    zip @Data.Text.Lazy.Text @_ @Data.Text.Lazy.Text @_ = Data.Text.Lazy.zip
"cons zip ltext eta" [~2]
    forall a b.
    zip @Data.Text.Lazy.Text @_ @Data.Text.Lazy.Text @_ a b = Data.Text.Lazy.zip a b

"cons zip vector" [~2]
    zip @(Vector _) @_ @(Vector _) @_ = Data.Vector.zip
"cons zip vector eta" [~2]
    forall a b.
    zip @(Vector _) @_ @(Vector _) @_ a b = Data.Vector.zip a b

"cons zip bs" [~2]
    zip @BS.ByteString @_ @BS.ByteString @_ = BS.zip
"cons zip bs eta" [~2]
    forall a b.
    zip @BS.ByteString @_ @BS.ByteString @_ a b = BS.zip a b

"cons zip lbs" [~2]
    zip @LBS.ByteString @_ @LBS.ByteString @_ = LBS.zip
"cons zip lbs eta" [~2]
    forall a b.
    zip @LBS.ByteString @_ @LBS.ByteString @_ a b = LBS.zip a b

"cons zip seq" [~2]
    zip @(Seq _) @_ @(Seq _) @_ = Data.Sequence.zip
"cons zip seq eta" [~2]
    forall a b.
    zip @(Seq _) @_ @(Seq _) @_ a b = Data.Sequence.zip a b
#-}


{-# inline [1] zip3 #-}
-- zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , AsEmpty v,  Cons v v (a,b,c) (a,b,c)
    )
  => s -> t -> u -> v
zip3 = go
  where
    go s t u =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) -> ((,,) a b c) `cons` go as bs cs

{-# rules
"cons zip3 vector" [~2]
    zip3 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_= Data.Vector.zip3
"cons zip3 vector eta" [~2]
    forall a b c.
    zip3 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ a b c = Data.Vector.zip3 a b c

"cons zip3 seq" [~2]
    zip3 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ = Data.Sequence.zip3
"cons zip3 seq eta" [~2]
    forall a b c.
    zip3 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ a b c = Data.Sequence.zip3 a b c
#-}


{-# inline [1] zip4 #-}
-- zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , AsEmpty w,  Cons w w (a,b,c,d) (a,b,c,d)
    )
  => s -> t -> u -> v -> w
zip4 = go
  where
    go s t u v =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) -> ((,,,) a b c d) `cons` go as bs cs ds

{-# rules
"cons zip4 vector" [~2]
    zip4 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ = Data.Vector.zip4
"cons zip4 vector eta" [~2]
    forall a b c d.
    zip4 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ a b c d = Data.Vector.zip4 a b c d

"cons zip4 seq" [~2]
    zip4 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ = Data.Sequence.zip4
"cons zip4 seq eta" [~2]
    forall a b c d.
    zip4 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ a b c d = Data.Sequence.zip4 a b c d
#-}


{-# inline [1] zip5 #-}
-- zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , Cons w w e e
    , AsEmpty x,  Cons x x (a,b,c,d,e) (a,b,c,d,e)
    )
  => s -> t -> u -> v -> w -> x
zip5 = go
  where
    go s t u v w =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) ->
                      case uncons w of
                        Nothing -> Empty
                        Just (e, es) -> ((,,,,) a b c d e) `cons` go as bs cs ds es

{-# rules
"cons zip5 vector" [~2]
    zip5 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ = Data.Vector.zip5
"cons zip5 vector eta" [~2]
    forall a b c d e.
    zip5 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ a b c d e = Data.Vector.zip5 a b c d e
#-}


{-# inline [1] zip6 #-}
-- zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e,f)]
zip6 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , Cons w w e e
    , Cons x x f f
    , AsEmpty y,  Cons y y (a,b,c,d,e,f) (a,b,c,d,e,f)
    )
  => s -> t -> u -> v -> w -> x -> y
zip6 = go
  where
    go s t u v w x =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) ->
                      case uncons w of
                        Nothing -> Empty
                        Just (e, es) ->
                          case uncons x of
                            Nothing -> Empty
                            Just (f, fs) -> ((,,,,,) a b c d e f) `cons` go as bs cs ds es fs

{-# rules
"cons zip6 vector" [~2]
    zip6 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ = Data.Vector.zip6
"cons zip6 vector eta" [~2]
    forall a b c d e f.
    zip6 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ a b c d e f = Data.Vector.zip6 a b c d e f
#-}


{-# inline [1] zip7 #-}
-- zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e,f,g)]
zip7 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , Cons w w e e
    , Cons x x f f
    , Cons y y g g
    , AsEmpty z,  Cons z z (a,b,c,d,e,f,g) (a,b,c,d,e,f,g)
    )
  => s -> t -> u -> v -> w -> x -> y -> z
zip7 = go
  where
    go s t u v w x y =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) ->
                      case uncons w of
                        Nothing -> Empty
                        Just (e, es) ->
                          case uncons x of
                            Nothing -> Empty
                            Just (f, fs) ->
                              case uncons y of
                                Nothing -> Empty
                                Just (g, gs) -> ((,,,,,,) a b c d e f g) `cons` go as bs cs ds es fs gs


{-# inline [1] zipWith #-}
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith ::
    ( Cons s s a a
    , Cons t t b b
    , AsEmpty u, Cons u u c c
    )
  => (a -> b -> c) -> s -> t -> u
zipWith f = go
  where
    go s t =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) -> f a b `cons` go as bs


{-# inline [0] zipWithFB #-}
zipWithFB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB c f = \x y r -> (x `f` y) `c` r

{-# rules
"cons zipWith" [~1]
    forall f xs ys.
    zipWith f xs ys = build (\c n -> foldr2 (zipWithFB c f) n xs ys)
"cons zipWithList" [1]
    forall f.
    foldr2 (zipWithFB (:) f) [] = zipWith f

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

"cons zipWith vector" [~2]
    zipWith @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ =
      Data.Vector.zipWith
"cons zipWith vector eta" [~2]
    forall f a b.
    zipWith @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ f a b =
      Data.Vector.zipWith f a b

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


{-# noinline [1] zipWith3 #-}
-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , AsEmpty v, Cons v v d d
    )
  => (a -> b -> c -> d) -> s -> t -> u -> v
zipWith3 f = go
  where
    go s t u =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) -> f a b c `cons` go as bs cs

{-# rules
"cons zipWith3 vector" [~2]
    zipWith3 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ =
      Data.Vector.zipWith3
"cons zipWith3 vector eta" [~2]
    forall f a b c.
    zipWith3 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ f a b c =
      Data.Vector.zipWith3 f a b c

"cons zipWith3 seq" [~2]
    zipWith3 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ =
      Data.Sequence.zipWith3
"cons zipWith3 seq eta" [~2]
    forall f a b c.
    zipWith3 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ f a b c =
      Data.Sequence.zipWith3 f a b c
#-}


{-# inline [1] zipWith4 #-}
-- zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , AsEmpty w, Cons w w e e
    )
  => (a -> b -> c -> d -> e) -> s -> t -> u -> v -> w
zipWith4 f = go
  where
    go s t u v =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) -> f a b c d `cons` go as bs cs ds

{-# rules
"cons zipWith4 vector" [~2]
    zipWith4 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ =
      Data.Vector.zipWith4
"cons zipWith4 vector eta" [~2]
    forall f a b c d.
    zipWith4 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ f a b c d =
      Data.Vector.zipWith4 f a b c d

"cons zipWith4 seq" [~2]
    zipWith4 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ =
      Data.Sequence.zipWith4
"cons zipWith4 seq eta" [~2]
    forall f a b c d.
    zipWith4 @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ @(Seq _) @_ f a b c d =
      Data.Sequence.zipWith4 f a b c d
#-}


{-# inline [1] zipWith5 #-}
-- zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , Cons w w e e
    , AsEmpty z, Cons z z f f
     )
  => (a -> b -> c -> d -> e -> f) -> s -> t -> u -> v -> w -> z
zipWith5 ff = go
  where
    go s t u v w =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) ->
                      case uncons w of
                        Nothing -> Empty
                        Just (e, es) -> ff a b c d e `cons` go as bs cs ds es

{-# rules
"cons zipWith5 vector" [~2]
    zipWith5 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ =
      Data.Vector.zipWith5
"cons zipWith5 vector eta" [~2]
    forall ff a b c d e.
    zipWith5 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ ff a b c d e =
      Data.Vector.zipWith5 ff a b c d e
#-}


{-# inline [1] zipWith6 #-}
-- zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith6 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , Cons w w e e
    , Cons z z f f
    , AsEmpty zz, Cons zz zz g g
    )
  => (a -> b -> c -> d -> e -> f -> g) -> s -> t -> u -> v -> w -> z -> zz
zipWith6 ff = go
  where
    go s t u v w z =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) ->
                      case uncons w of
                        Nothing -> Empty
                        Just (e, es) ->
                          case uncons z of
                            Nothing -> Empty
                            Just (f, fs) -> ff a b c d e f `cons` go as bs cs ds es fs

{-# rules
"cons zipWith6 vector" [~2]
    zipWith6 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ =
      Data.Vector.zipWith6
"cons zipWith6 vector eta" [~2]
    forall ff a b c d e f.
    zipWith6 @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ @(Vector _) @_ ff a b c d e f =
      Data.Vector.zipWith6 ff a b c d e f
#-}


{-# inline [1] zipWith7 #-}
-- zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipWith7 ::
    ( Cons s s a a
    , Cons t t b b
    , Cons u u c c
    , Cons v v d d
    , Cons w w e e
    , Cons z z f f
    , Cons zz zz g g
    , AsEmpty zzz, Cons zzz zzz h h
    )
  => (a -> b -> c -> d -> e -> f -> g -> h) -> s -> t -> u -> v -> w -> z -> zz -> zzz
zipWith7 ff = go
  where
    go s t u v w z zz =
      case uncons s of
        Nothing -> Empty
        Just (a, as) ->
          case uncons t of
            Nothing -> Empty
            Just (b, bs) ->
              case uncons u of
                Nothing -> Empty
                Just (c, cs) ->
                  case uncons v of
                    Nothing -> Empty
                    Just (d, ds) ->
                      case uncons w of
                        Nothing -> Empty
                        Just (e, es) ->
                          case uncons z of
                            Nothing -> Empty
                            Just (f, fs) ->
                              case uncons zz of
                                Nothing -> Empty
                                Just (g, gs) -> ff a b c d e f g `cons` go as bs cs ds es fs gs


{-# inline [1] unzip #-}
-- unzip :: [(a,b)] -> ([a],[b])
unzip ::
    ( AsEmpty s, Cons s s (a,b) (a,b)
    , AsEmpty t, Cons t t a a
    , AsEmpty u, Cons u u b b
    )
  => s -> (t,u)
unzip =  foldr (\(a,b) ~(as,bs) -> (a `cons` as, b `cons` bs)) (Empty,Empty)

{-# rules
"cons unzip vector" [~2]
    unzip @(Vector (_,_)) = Data.Vector.unzip
"cons unzip vector eta" [~2]
    forall a.
    unzip @(Vector (_,_)) a = Data.Vector.unzip a

"cons unzip bs" [~2]
    unzip @[(Word8,Word8)] = BS.unzip
"cons unzip bs eta" [~2]
    forall a.
    unzip @[(Word8,Word8)] a = BS.unzip a

"cons unzip lbs" [~2]
    unzip @[(Word8,Word8)] = LBS.unzip
"cons unzip lbs eta" [~2]
    forall a.
    unzip @[(Word8,Word8)] a = LBS.unzip a
#-}


{-# inline [1] unzip3 #-}
-- unzip3 :: [(a,b,c)] -> ([a],[b],[c])
unzip3 ::
    ( AsEmpty s, Cons s s (a,b,c) (a,b,c)
    , AsEmpty t, Cons t t a a
    , AsEmpty u, Cons u u b b
    , AsEmpty v, Cons v v c c
    )
  => s -> (t,u,v)
unzip3 =  foldr (\(a,b,c) ~(as,bs,cs) -> (a `cons` as, b `cons` bs, c `cons` cs)) (Empty,Empty,Empty)

{-# rules
"cons unzip3 vector" [~2]
    unzip3 @(Vector (_,_,_)) = Data.Vector.unzip3
"cons unzip3 vector eta" [~2]
    forall a.
    unzip3 @(Vector (_,_,_)) a = Data.Vector.unzip3 a
#-}


{-# inline [1] unzip4 #-}
-- unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 ::
    ( AsEmpty s, Cons s s (a,b,c,d) (a,b,c,d)
    , AsEmpty t, Cons t t a a
    , AsEmpty u, Cons u u b b
    , AsEmpty v, Cons v v c c
    , AsEmpty w, Cons w w d d
    )
  => s -> (t,u,v,w)
unzip4 =  foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a `cons` as, b `cons` bs, c `cons` cs, d `cons` ds)) (Empty,Empty,Empty,Empty)

{-# rules
"cons unzip4 vector" [~2]
    unzip4 @(Vector (_,_,_,_)) = Data.Vector.unzip4
"cons unzip4 vector eta" [~2]
    forall a.
    unzip4 @(Vector (_,_,_,_)) a = Data.Vector.unzip4 a
#-}


{-# inline [1] unzip5 #-}
-- unzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5 ::
    ( AsEmpty s, Cons s s (a,b,c,d,e) (a,b,c,d,e)
    , AsEmpty t, Cons t t a a
    , AsEmpty u, Cons u u b b
    , AsEmpty v, Cons v v c c
    , AsEmpty w, Cons w w d d
    , AsEmpty x, Cons x x e e
    )
  => s -> (t,u,v,w,x)
unzip5 =  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) -> (a `cons` as, b `cons` bs, c `cons` cs, d `cons` ds, e `cons` es)) (Empty,Empty,Empty,Empty,Empty)

{-# rules
"cons unzip5 vector" [~2]
    unzip5 @(Vector (_,_,_,_,_)) = Data.Vector.unzip5
"cons unzip5 vector eta" [~2]
    forall a.
    unzip5 @(Vector (_,_,_,_,_)) a = Data.Vector.unzip5 a
#-}


{-# inline [1] unzip6 #-}
-- unzip6 :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6 ::
    ( AsEmpty s, Cons s s (a,b,c,d,e,f) (a,b,c,d,e,f)
    , AsEmpty t, Cons t t a a
    , AsEmpty u, Cons u u b b
    , AsEmpty v, Cons v v c c
    , AsEmpty w, Cons w w d d
    , AsEmpty x, Cons x x e e
    , AsEmpty y, Cons y y f f
    )
  => s -> (t,u,v,w,x,y)
unzip6 =  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) -> (a `cons` as, b `cons` bs, c `cons` cs, d `cons` ds, e `cons` es, f `cons` fs)) (Empty,Empty,Empty,Empty,Empty,Empty)

{-# rules
"cons unzip6 vector" [~2]
    unzip6 @(Vector (_,_,_,_,_,_)) = Data.Vector.unzip6
"cons unzip6 vector eta" [~2]
    forall a.
    unzip6 @(Vector (_,_,_,_,_,_)) a = Data.Vector.unzip6 a
#-}


{-# inline [1] unzip7 #-}
-- unzip7 :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7 ::
    ( AsEmpty s, Cons s s (a,b,c,d,e,f,g) (a,b,c,d,e,f,g)
    , AsEmpty t, Cons t t a a
    , AsEmpty u, Cons u u b b
    , AsEmpty v, Cons v v c c
    , AsEmpty w, Cons w w d d
    , AsEmpty x, Cons x x e e
    , AsEmpty y, Cons y y f f
    , AsEmpty z, Cons z z g g
    )
  => s -> (t,u,v,w,x,y,z)
unzip7 =  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) -> (a `cons` as, b `cons` bs, c `cons` cs, d `cons` ds, e `cons` es, f `cons` fs, g `cons` gs)) (Empty,Empty,Empty,Empty,Empty,Empty,Empty)
