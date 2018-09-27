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
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Consy.Transformations
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , reverse
  , intersperse
  , intercalate
  , transpose
  , subsequences
  )
where

import Control.Lens
import Control.Lens.Cons
import Control.Lens.Empty
import Data.Function ((.))
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Tuple (fst, snd)
import Data.Vector (Vector)
import GHC.Base (flip, id)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Functor
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy as TL
import qualified Data.Vector

import Consy.Folds (build, foldl, foldr)
import Consy.SpecialFolds (concat, concatMap)

{-# inline [2] reverse #-}
reverse :: forall s a. (AsEmpty s, Cons s s a a) => s -> s
reverse = foldl (flip cons) Empty

{-# rules
"cons reverse text" [~2]
    reverse @Text = Data.Text.reverse
"cons reverse text eta" [~2]
    forall a.
    reverse @Text a = Data.Text.reverse a

"cons reverse ltext" [~2]
    reverse @TL.Text = TL.reverse
"cons reverse ltext eta" [~2]
    forall a.
    reverse @TL.Text a = TL.reverse a

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
intersperse :: (AsEmpty s, Cons s s a a) => a -> s -> s
intersperse = \sep s ->
  case uncons s of
    Nothing -> Empty
    Just (x, xs) -> x `cons` prependToAll sep xs
  where
    prependToAll sep s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) -> sep `cons` x `cons` prependToAll sep xs

{-# rules
"cons intersperse text" [~2]
    intersperse @Text = Data.Text.intersperse
"cons intersperse text eta" [~2]
    forall a as.
    intersperse @Text a as = Data.Text.intersperse a as

"cons intersperse ltext" [~2]
    intersperse @TL.Text = TL.intersperse
"cons intersperse ltext eta" [~2]
    forall a as.
    intersperse @TL.Text a as = TL.intersperse a as

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
intercalate = \xs xss -> concat (intersperse xs xss)

{-# rules
"cons intercalate text" [~2]
    intercalate @Text = Data.Text.intercalate
"cons intercalate text eta" [~2]
    forall xs xss.
    intercalate @Text xs xss = Data.Text.intercalate xs xss

"cons intercalate ltext" [~2]
    intercalate @TL.Text = TL.intercalate
"cons intercalate ltext eta" [~2]
    forall xs xss.
    intercalate @TL.Text xs xss = TL.intercalate xs xss

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
transpose
  :: forall s v t u a
   . ( AsEmpty s, AsEmpty u, AsEmpty v
     , Cons s s t t, Cons t t a a
     , Cons v v u u, Cons u u a a
     )
  => s -> v
transpose = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (a, xss) ->
          case uncons a of
            Nothing -> go xss
            Just (x, xs) ->
              (x `cons` allHeads xss) `cons`
              go ((xs `cons` allTails xss) :: s)

    -- allHeads :: (AsEmpty y, Cons y y z z, Cons x x w w, Cons w w z z) => x -> y
    allHeads t =
      case uncons t of
        Nothing -> Empty
        Just (x, xs) ->
          case uncons x of
            Nothing -> allHeads xs
            Just (a, _) -> a `cons` allHeads xs

    -- allTails :: (AsEmpty y, Cons y y z z, Cons z z w w, Cons x x z z) => x -> y
    allTails t =
      case uncons t of
        Nothing -> Empty
        Just (x, xs) ->
          case uncons x of
            Nothing -> allTails xs
            Just (_, a) -> a `cons` allTails xs

{-# rules
"cons transpose text" [~2]
    transpose @[Text] @[Text] = Data.Text.transpose
"cons transpose text eta" [~2]
    forall xss.
    transpose @[Text] @[Text] xss = Data.Text.transpose xss

"cons transpose ltext" [~2]
    transpose @[TL.Text] @[TL.Text] = TL.transpose
"cons transpose ltext eta" [~2]
    forall xss.
    transpose @[TL.Text] @[TL.Text] xss = TL.transpose xss

"cons transpose bs" [~2]
    transpose @[BS.ByteString] @[BS.ByteString] = BS.transpose
"cons transpose bs eta" [~2]
    forall xss.
    transpose @[BS.ByteString] @[BS.ByteString] xss = BS.transpose xss

"cons transpose lbs" [~2]
    transpose @[LBS.ByteString] @[LBS.ByteString] = LBS.transpose
"cons transpose lbs eta" [~2]
    forall xss.
    transpose @[LBS.ByteString] @[LBS.ByteString] xss = LBS.transpose xss
#-}


-- subsequences :: [a] -> [[a]]
subsequences :: (AsEmpty s, Cons s s a a) => s -> [s]
subsequences s = Empty : nonEmptySubsequences s

-- nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences :: (AsEmpty s, Cons s s a a) => s -> [s]
nonEmptySubsequences = go
  where
    go s =
      case uncons s of
        Nothing -> Empty
        Just (x, xs) ->
          (x `cons` Empty)
          :
          foldr f Empty (go xs)
            where f ys r = ys : (x `cons` ys) : r

-- Note: not in Text, Lazy Text, nor in BS, LBS
