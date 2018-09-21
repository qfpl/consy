{-
== List transformations ==
+ map
-}

{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
-- {-# language AllowAmbiguousTypes #-}
-- {-# language BangPatterns #-}
-- {-# language FlexibleContexts #-}
-- {-# language PatternSynonyms #-}
-- {-# language RankNTypes #-}
-- {-# language ScopedTypeVariables #-}
module Consy.TransformationsMap
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , map
  )
where

import Control.Lens
import Control.Lens.Cons
import Control.Lens.Empty
import Data.Function ((.))
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Functor
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds (build, foldr)


{- ___ List transformations _________________________________________________ -}

{-# inline [0] map #-}
-- map ::  (a -> b) -> [a] -> [b]
map :: (AsEmpty s, Cons s s a a) => (a -> a) -> s -> s
map f = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) -> f x `cons` go xs

{-# inline [0] mapFB #-}
-- mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapFB c f = \x ys -> c (f x) ys

{-# rules
"cons map" [~1]
    forall f xs.
    map f xs = build (\c n -> foldr (mapFB c f) n xs)
"cons mapList list" [1]
    forall f.
    foldr (mapFB (:) f) [] = map f

"cons mapFB"
    forall c f g.
    mapFB (mapFB c f) g = mapFB c (f.g)
"cons mapFB/id"
    forall c.
    mapFB c (\x -> x) = c

"cons map ltext"
    map @Data.Text.Lazy.Text = Data.Text.Lazy.map
"cons map ltext eta"
    forall f xs.
    map @Data.Text.Lazy.Text f xs = Data.Text.Lazy.map f xs

"cons map text"
    map @Text = Data.Text.map
"cons map text eta"
    forall f xs.
    map @Text f xs = Data.Text.map f xs

"cons map vector"
    map @(Vector _) = Data.Vector.map
"cons map vector eta"
    forall f xs.
    map @(Vector _) f xs = Data.Vector.map f xs

"cons map bs"
    map @BS.ByteString = BS.map
"cons map bs eta"
    forall f xs. map @BS.ByteString f xs = BS.map f xs

"cons map bslazy"
    map @LBS.ByteString = LBS.map
"cons map bslazy eta"
    forall f xs.
    map @LBS.ByteString f xs = LBS.map f xs

"cons map seq"
    map @(Seq _) = Data.Functor.fmap
"cons map seq eta"
    forall f xs.
    map @(Seq _) f xs = Data.Functor.fmap f xs
#-}
