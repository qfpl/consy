{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language PatternSynonyms #-}
{-# language TypeApplications #-}
module Consy.Scans where

import Control.Lens.Cons (Cons, cons, uncons)
import Control.Lens.Empty (AsEmpty, pattern Empty)
import Data.Function (const)
import Data.Maybe (Maybe(..))
import GHC.Base (oneShot)

import Consy.Basic (tail)
import Consy.Folds (foldr, build)

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector


{-# noinline [1] scanl #-}
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t b b) => (b -> a -> b) -> b -> s -> t
scanl = scanlGo
  where
    scanlGo f q ls =
      q `cons`
      case uncons ls of
        Nothing -> Empty
        Just (x, xs) -> scanlGo f (f q x) xs

{-# rules
"scanl cons" [~1]
    forall f a bs.
    scanl f a bs = build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)

"scanlList cons" [1]
    forall (f::a->b->a) (a::a) (bs::[b]).
    foldr (scanlFB f (:)) (constScanl []) bs a = tail (scanl f a bs)
#-}

{-# rules
"scanl cons text" [~2]
    scanl @Data.Text.Text = Data.Text.scanl

"scanl cons text eta" [~2]
    forall a b c.
    scanl @Data.Text.Text a b c = Data.Text.scanl a b c

"scanl cons text lazy" [~2]
    scanl @Data.Text.Lazy.Text = Data.Text.Lazy.scanl

"scanl cons text lazy eta" [~2]
    forall a b c.
    scanl @Data.Text.Lazy.Text a b c = Data.Text.Lazy.scanl a b c

"scanl cons bs" [~2]
    scanl @Data.ByteString.ByteString = Data.ByteString.scanl

"scanl cons bs eta" [~2]
    forall a b c.
    scanl @Data.ByteString.ByteString a b c = Data.ByteString.scanl a b c

"scanl cons bs lazy" [~2]
    scanl @Data.ByteString.Lazy.ByteString = Data.ByteString.Lazy.scanl

"scanl cons bs lazy eta" [~2]
    forall a b c.
    scanl @Data.ByteString.Lazy.ByteString a b c = Data.ByteString.Lazy.scanl a b c

"scanl cons vector" [~2]
    scanl @(Data.Vector.Vector _) = Data.Vector.scanl

"scanl cons vector eta" [~2]
    forall a b c.
    scanl @(Data.Vector.Vector _) a b c = Data.Vector.scanl a b c
#-}

{-# inline [0] scanlFB #-}
scanlFB :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB f c = \b g -> oneShot (\x -> let b' = f x b in b' `c` g b')

{-# inline [0] constScanl #-}
constScanl :: a -> b -> a
constScanl = const


{-# noinline [1] scanl' #-}
-- scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t b b) => (b -> a -> b) -> b -> s -> t
scanl' = scanlGo'
  where
    scanlGo' f !q ls =
      q `cons`
      case uncons ls of
        Nothing -> Empty
        Just (x, xs) -> scanlGo' f (f q x) xs

{-# rules
"scanl' cons vector" [~2]
    scanl' @(Data.Vector.Vector _) = Data.Vector.scanl'

"scanl' cons vector eta" [~2]
    forall a b c.
    scanl' @(Data.Vector.Vector _) a b c = Data.Vector.scanl' a b c
#-}

{-# rules
"scanl' cons" [~1]
    forall f a bs . scanl' f a bs =
    build (\c n -> a `c` foldr (scanlFB' f c) (flipSeqScanl' n) bs a)
"scanlList' cons" [1]
    forall (f::b->a->b) a (bs::[a]).
    foldr (scanlFB' f (:)) (flipSeqScanl' []) bs a = tail (scanl' f a bs)
#-}

{-# inline [0] scanlFB' #-}
scanlFB' :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB' f c = \b g -> oneShot (\x -> let !b' = f x b in b' `c` g b')

{-# inline [0] flipSeqScanl' #-}
flipSeqScanl' :: a -> b -> a
flipSeqScanl' a !_b = a


{-# inline [2] scanl1 #-}
-- scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 :: (AsEmpty s, Cons s s a a) => (a -> a -> a) -> s -> s
scanl1 = \f a ->
  case uncons a of
    Just (x, xs) -> scanl f x xs
    Nothing -> Empty

{-# rules
"scanl1 cons text" [~2]
    scanl1 @Data.Text.Text = Data.Text.scanl1

"scanl1 cons text eta" [~2]
    forall a b.
    scanl1 @Data.Text.Text a b = Data.Text.scanl1 a b

"scanl1 cons text lazy" [~2]
    scanl1 @Data.Text.Lazy.Text = Data.Text.Lazy.scanl1

"scanl1 cons text lazy eta" [~2]
    forall a b.
    scanl1 @Data.Text.Lazy.Text a b = Data.Text.Lazy.scanl1 a b

"scanl1 cons bs" [~2]
    scanl1 @Data.ByteString.ByteString = Data.ByteString.scanl1

"scanl1 cons bs eta" [~2]
    forall a b.
    scanl1 @Data.ByteString.ByteString a b = Data.ByteString.scanl1 a b

"scanl1 cons vector" [~2]
    scanl1 @(Data.Vector.Vector _) = Data.Vector.scanl1

"scanl1 cons vector eta" [~2]
    forall a b.
    scanl1 @(Data.Vector.Vector _) a b = Data.Vector.scanl1 a b
#-}


{-# noinline [1] scanr #-}
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr :: (AsEmpty s, Cons s s a a, AsEmpty t, Cons t t b b) => (a -> b -> b) -> b -> s -> t
scanr f q0 = go
  where
    go a =
      case uncons a of
        Nothing -> q0 `cons` Empty
        Just (x, xs) -> f x q `cons` qs
          where
            qs = go xs
            Just (q, _) = uncons qs

{-# inline [0] strictUncurryScanr #-}
strictUncurryScanr :: (a -> b -> c) -> (a, b) -> c
strictUncurryScanr f pair = case pair of
                              (x, y) -> f x y

{-# inline [0] scanrFB #-}
scanrFB :: (a -> b -> b) -> (b -> c -> c) -> a -> (b, c) -> (b, c)
scanrFB f c = \x (r, est) -> (f x r, r `c` est)

{-# rules
"scanr" [~1]
    forall f q0 (ls::[a]).
    scanr f q0 ls =
      build (\c n -> strictUncurryScanr c (foldr (scanrFB f c) (q0,n) ls))
"scanrList" [1]
    forall f q0 (ls::[a]).
      strictUncurryScanr (:) (foldr (scanrFB f (:)) (q0,[]) ls) = scanr f q0 ls
#-}

{-# rules
"scanr cons text" [~2]
    scanr @Data.Text.Text = Data.Text.scanr

"scanr cons text eta" [~2]
    forall a b c.
    scanr @Data.Text.Text a b c = Data.Text.scanr a b c

"scanr cons text lazy" [~2]
    scanr @Data.Text.Lazy.Text = Data.Text.Lazy.scanr

"scanr cons text lazy eta" [~2]
    forall a b c.
    scanr @Data.Text.Lazy.Text a b c = Data.Text.Lazy.scanr a b c

"scanr cons bs" [~2]
    scanr @Data.ByteString.ByteString = Data.ByteString.scanr

"scanr cons bs eta" [~2]
    forall a b c.
    scanr @Data.ByteString.ByteString a b c = Data.ByteString.scanr a b c

"scanr cons vector" [~2]
    scanr @(Data.Vector.Vector _) = Data.Vector.scanr

"scanr cons vector eta" [~2]
    forall a b c.
    scanr @(Data.Vector.Vector _) a b c = Data.Vector.scanr a b c
#-}


{-# inline [2] scanr1 #-}
-- scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 :: (AsEmpty s, Cons s s a a) => (a -> a -> a) -> s -> s
scanr1 f = go
  where
    go a =
      case uncons a of
        Nothing -> Empty
        Just (x, xs) ->
          case uncons xs of
            Nothing -> x `cons` Empty
            _ -> f x q `cons` qs
              where
                qs = go xs
                Just (q, _) = uncons qs

{-# rules
"scanr1 cons text" [~2]
    scanr1 @Data.Text.Text = Data.Text.scanr1

"scanr1 cons text eta" [~2]
    forall a b.
    scanr1 @Data.Text.Text a b = Data.Text.scanr1 a b

"scanr1 cons text lazy" [~2]
    scanr1 @Data.Text.Lazy.Text = Data.Text.Lazy.scanr1

"scanr1 cons text lazy eta" [~2]
    forall a b.
    scanr1 @Data.Text.Lazy.Text a b = Data.Text.Lazy.scanr1 a b

"scanr1 cons bs" [~2]
    scanr1 @Data.ByteString.ByteString = Data.ByteString.scanr1

"scanr1 cons bs eta" [~2]
    forall a b.
    scanr1 @Data.ByteString.ByteString a b = Data.ByteString.scanr1 a b

"scanr1 cons vector" [~2]
    scanr1 @(Data.Vector.Vector _) = Data.Vector.scanr1

"scanr1 cons vector eta" [~2]
    forall a b.
    scanr1 @(Data.Vector.Vector _) a b = Data.Vector.scanr1 a b
#-}
