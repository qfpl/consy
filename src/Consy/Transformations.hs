{-
== List transformations ==
+ map
+ reverse
+ intersperse
+ intercalate
+ transpose
+ subsequences
permutations
-}

{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
-- {-# language RankNTypes #-}
-- {-# language PatternSynonyms #-}
-- {-# language ScopedTypeVariables #-}
-- {-# language BangPatterns #-}
-- {-# language FlexibleContexts #-}
-- {-# language AllowAmbiguousTypes #-}
module Consy.Transformations
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  -- , map
  , reverse
  , intersperse
  , intercalate
  , transpose
  , subsequences
  -- , permutations
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
import GHC.Base (flip, id)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Functor
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds (build, foldl, foldr)
import Consy.SpecialFolds (concat, concatMap)


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


{-# inline [2] reverse #-}
-- reverse ::  [a] -> [a]
reverse :: (AsEmpty s, Cons s s a a) => s -> s
reverse = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) -> foldl (flip (cons)) Empty s
        --f x `cons` go xs

{-# rules
"cons reverse text" [~2]
    reverse @Text = Data.Text.reverse
"cons reverse text eta" [~2]
    forall a.
    reverse @Text a = Data.Text.reverse a

"cons reverse ltext" [~2]
    reverse @Data.Text.Lazy.Text = Data.Text.Lazy.reverse
"cons reverse ltext eta" [~2]
    forall a.
    reverse @Data.Text.Lazy.Text a = Data.Text.Lazy.reverse a

"cons reverse vector" [~2]
    reverse @(Vector _) = Data.Vector.reverse
"cons reverse vector eta" [~2]
    forall a.
    reverse @(Vector _) a = Data.Vector.reverse a

"cons reverse bs" [~2]
    reverse @BS.ByteString = BS.reverse
"cons reverse bs eta" [~2]
    forall a.
    reverse @BS.ByteString a = BS.reverse a

"cons reverse lbs" [~2]
    reverse @LBS.ByteString = LBS.reverse
"cons reverse lbs eta" [~2]
    forall a.
    reverse @LBS.ByteString a = LBS.reverse a

"cons reverse seq" [~2]
    reverse @(Seq _) = Data.Sequence.reverse
"cons reverse seq eta" [~2]
    forall a.
    reverse @(Seq _) a = Data.Sequence.reverse a
#-}


{-# inline [2] intersperse #-}
-- intersperse :: a -> [a] -> [a]
intersperse :: (AsEmpty s, Cons s s a a) => a -> s -> s
intersperse sep = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) -> x `cons` prependToAll sep xs

prependToAll :: (AsEmpty s, Cons s s a a) => a -> s -> s
prependToAll sep = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) -> sep `cons` x `cons` go xs

{-# rules
"cons intersperse text" [~2]
    intersperse @Text = Data.Text.intersperse
"cons intersperse text eta" [~2]
    forall a as.
    intersperse @Text a as = Data.Text.intersperse a as

"cons intersperse ltext" [~2]
    intersperse @Data.Text.Lazy.Text = Data.Text.Lazy.intersperse
"cons intersperse ltext eta" [~2]
    forall a as.
    intersperse @Data.Text.Lazy.Text a as = Data.Text.Lazy.intersperse a as

"cons intersperse bs" [~2]
    intersperse @BS.ByteString = BS.intersperse
"cons intersperse bs eta" [~2]
    forall a as.
    intersperse @BS.ByteString a as = BS.intersperse a as

"cons intersperse lbs" [~2]
    intersperse @LBS.ByteString = LBS.intersperse
"cons intersperse lbs eta" [~2]
    forall a as.
    intersperse @LBS.ByteString a as= LBS.intersperse a as
#-}


{-# inline [2] intercalate #-}
-- intercalate :: [a] -> [[a]] -> [a]
intercalate :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t s s) => s -> t -> s
intercalate xs xss = concat (intersperse xs xss)

{-# rules
"cons intercalate text" [~2]
    intercalate @Text = Data.Text.intercalate
"cons intercalate text eta" [~2]
    forall xs xss.
    intercalate @Text xs xss = Data.Text.intercalate xs xss

"cons intercalate ltext" [~2]
    intercalate @Data.Text.Lazy.Text = Data.Text.Lazy.intercalate
"cons intercalate ltext eta" [~2]
    forall xs xss.
    intercalate @Data.Text.Lazy.Text xs xss = Data.Text.Lazy.intercalate xs xss

"cons intercalate bs" [~2]
    intercalate @BS.ByteString = BS.intercalate
"cons intercalate bs eta" [~2]
    forall xs xss.
    intercalate @BS.ByteString xs xss = BS.intercalate xs xss

"cons intercalate lbs" [~2]
    intercalate @LBS.ByteString = LBS.intercalate
"cons intercalate lbs eta" [~2]
    forall xs xss.
    intercalate @LBS.ByteString xs xss= LBS.intercalate xs xss
#-}


{-# inline [2] transpose #-}
-- transpose :: [[a]] -> [[a]]
transpose :: (AsEmpty s, AsEmpty p, Cons s s a a, Cons p p s s) => p -> p
transpose s =
      case uncons s of
        Nothing -> Empty
        Just (Empty, xss) -> transpose xss
        Just (x_xs, xss) -> -- type of p
          let
            x = x_xs ^?! _head
            xs = x_xs ^?! _tail
          in
          (headTranspose x xss) `cons` (transpose (tailTranspose xs xss))
      where
        headTranspose x xss =
          -- x `cons` (concatMap (\x_xss -> (x_xss ^?! _head) `cons` Empty)) xss
          x `cons`
            (concatMap
              (\x_xss -> case (x_xss ^? _head) of
                          Nothing -> Empty
                          Just x -> x `cons` Empty
              )
              xss
            )

        tailTranspose xs xss =
          -- xs `cons` (concatMap (\xs_xss -> (xs_xss ^?! _tail) `cons` Empty)) xss
          xs `cons`
            (concatMap
              (\xs_xss -> (safeTail xs_xss) `cons` Empty)
              xss
            )

        safeTail xs_xss = case (xs_xss ^? _tail) of
          Nothing -> Empty
          Just xs -> xs

{-# rules
"cons transpose text" [~2]
    transpose @Text = Data.Text.transpose
"cons transpose text eta" [~2]
    forall xss.
    transpose @Text xss = Data.Text.transpose xss

"cons transpose ltext" [~2]
    transpose @Data.Text.Lazy.Text = Data.Text.Lazy.transpose
"cons transpose ltext eta" [~2]
    forall xss.
    transpose @Data.Text.Lazy.Text xss = Data.Text.Lazy.transpose xss

"cons transpose bs" [~2]
    transpose @BS.ByteString = BS.transpose
"cons transpose bs eta" [~2]
    forall xss.
    transpose @BS.ByteString xss = BS.transpose xss

"cons transpose lbs" [~2]
    transpose @LBS.ByteString = LBS.transpose
"cons transpose lbs eta" [~2]
    forall xss.
    transpose @LBS.ByteString xss= LBS.transpose xss
#-}


{-# inline [2] subsequences #-}
-- subsequences :: [a] -> [[a]]
subsequences :: (AsEmpty s, AsEmpty p, Cons s s a a, Cons p p s s) => s -> p
subsequences s = Empty `cons` nonEmptySubsequences s

-- nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences :: (AsEmpty s, AsEmpty p, Cons s s a a, Cons p p s s) => s -> p
nonEmptySubsequences = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) ->
          (x `cons` Empty)
          `cons`
          foldr f Empty (go xs)
            where f ys r = ys `cons` (x `cons` ys) `cons` r

-- Note: not in Text, Lazy Text, nor in BS, LBS


-- FAILS, not compiling
--{-# inline [2] permutations #-}
-- -- permutations :: [a] -> [[a]]
-- permutations :: (AsEmpty s, AsEmpty p, Cons s s a a, Cons p p s s) => s -> p
-- permutations xs0 = xs0 `cons` (perms xs0 Empty)
--   where
--     -- perms :: (AsEmpty s, AsEmpty p, Cons s s a a, Cons p p s s) => s -> s -> p
--     perms tts is =
--       case uncons tts of
--         Nothing -> Empty
--         Just (t, ts) -> foldr interleave (perms ts (t `cons` is)) (permutations is)
--           where
--             -- interleave :: (AsEmpty s, AsEmpty p, Cons s s a a, Cons p p s s) => s -> p -> p
--             interleave xs r =
--               let
--                 (_,zs) = interleave' id xs r
--               in
--                 zs
--             -- interleave' :: (AsEmpty s, AsEmpty p, Cons s s a a, Cons p p s s) => (s -> s) -> s -> p -> (s, p)
--             interleave' f yys r =
--               case uncons yys of
--                 Nothing -> (ts, r)
--                 Just (y,ys) ->
--                   let
--                     (us,zs) = interleave' (f . (y `cons`)) ys r
--                   in
--                     (y `cons` us, f (t `cons` y `cons` us) `cons` zs)


--
-- permutations xs0  =  xs0 : perms xs0 []
--   where
    -- perms []     _  = []
    -- perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
    --   where
    --     interleave    xs     r = let (_,zs) = interleave' id xs r in zs
    --     interleave' _ []     r = (ts, r)
    --     interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
    --                                  in  (y:us, f (t:y:us) : zs)

-- Note: not in Text, Lazy Text, BS, LBS
