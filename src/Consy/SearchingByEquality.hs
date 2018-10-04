{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.SearchingByEquality
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , elem
  , notElem
  , lookup
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), (&&), (||), otherwise)
import Data.Char (Char)
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Real (Integral, fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Vector

import Consy.Folds (build)


{-# noinline [1] elem #-}
-- elem :: (Eq a) => a -> [a] -> Bool
elem :: (Eq a, AsEmpty s, Cons s s a a) => a -> s -> Bool
-- elem x = any (== x)
elem x = go x
  where
    go x s =
      case uncons s of
        Nothing -> False
        Just (a, as) -> x == a || go x as

{-# rules
"cons elem/build"
    forall x (g :: forall b . Eq a => (a -> b -> b) -> b -> b).
    elem x (build g) = g (\ y r -> (x == y) || r) False

"cons elem vector"
    elem @_ @(Vector _) = Data.Vector.elem
"cons elem vector eta"
    forall x y.
    elem @(Vector _) x y = Data.Vector.elem x y

"cons elem bs"
    elem @Word8 @BS.ByteString = BS.elem
"cons elem bs eta"
    forall x y.
    elem @Word8 @BS.ByteString x y = BS.elem x y

"cons elem bslazy"
    elem @Word8 @LBS.ByteString = LBS.elem
"cons elem bslazy eta"
    forall x y.
    elem @Word8 @LBS.ByteString x y = LBS.elem x y
#-}


{-# noinline [1] notElem #-}
-- notElem :: (Eq a) => a -> [a] -> Bool
notElem :: (Eq a, AsEmpty s, Cons s s a a) => a -> s -> Bool
notElem x = go x
  where
    go x s =
      case uncons s of
        Nothing -> True
        Just (a, as) -> x /= a && go x as

{-# rules
"cons notElem/build"
    forall x (g :: forall b . Eq a => (a -> b -> b) -> b -> b).
    notElem x (build g) = g (\ y r -> (x /= y) && r) True

"cons notElem vector"
    notElem @_ @(Vector _) = Data.Vector.notElem
"cons notElem vector eta"
    forall x y.
    notElem @(Vector _) x y = Data.Vector.notElem x y

"cons notElem bs"
    notElem @Word8 @BS.ByteString = BS.notElem
"cons notElem bs eta"
    forall x y.
    notElem @Word8 @BS.ByteString x y = BS.notElem x y

"cons notElem bslazy"
    notElem @Word8 @LBS.ByteString = LBS.notElem
"cons notElem bslazy eta"
    forall x y.
    notElem @Word8 @LBS.ByteString x y = LBS.notElem x y
#-}


{-# noinline [1] lookup #-}
-- lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup :: (Eq a, AsEmpty s, Cons s s (a,b) (a,b)) => a -> s -> Maybe b
lookup = go
  where
    go key s =
      case uncons s of
        Empty -> Nothing
        Just ((a,b), as)
          | a == key -> Just b
          | otherwise -> go key as
