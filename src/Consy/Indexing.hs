{-
== Indexing Sublists ==
+ (!!)
+ elemIndex
+ elemIndices
+ findIndex
+ findIndices
-}

{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
{-# LANGUAGE MagicHash #-}
-- {-# language PatternSynonyms #-}
-- {-# language RankNTypes #-}
-- {-# language ScopedTypeVariables #-}
-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE Trustworthy #-}
module Consy.Indexing
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , (!!)
  , elemIndex
  , elemIndices
  , findIndex
  , findIndices
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), otherwise)
import Data.Char (Char)
import Data.Eq (Eq(..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.Maybe (Maybe(..), listToMaybe)
import Data.Ord ((<))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (Int( I# ), (+#), errorWithoutStackTrace)
import GHC.Num ((-))
import GHC.Real (Integral, fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds (build, foldr)
import Consy.TransformationsMap (map)


{- ___ Indexing Sublists ____________________________________________________ -}

{-# inline [~1]  (!!) #-}
-- (!!) :: [a] -> Int -> a
(!!) :: (AsEmpty s, Cons s s a a ) => s -> Int -> a
(!!) =
  \s !n ->
    if n < 0
    then errorWithoutStackTrace "Prelude.!!: negative index"
    else
      foldr
        (\x r k ->
            case k of
              0 -> x
              _ -> r (k-1))
        tooLarge
        s
        n
  where
    tooLarge :: Int -> a
    tooLarge _ = errorWithoutStackTrace "Prelude.!!: index too large"


{-# inline [2] elemIndex #-}
-- elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex :: (AsEmpty s, Cons s s a a, Eq a) => a -> s -> Maybe Int
elemIndex x = findIndex (x==)

{-# rules
"cons elemIndex vector"
    elemIndex @(Vector _) = Data.Vector.elemIndex
"cons elemIndex vector eta"
    forall x xs.
    elemIndex @(Vector _) x xs = Data.Vector.elemIndex x xs

"cons elemIndex bs"
    elemIndex @BS.ByteString = BS.elemIndex
"cons elemIndex bs eta"
    forall x xs.
    elemIndex @BS.ByteString x xs = BS.elemIndex x xs

"cons elemIndex bslazy"
    elemIndex @LBS.ByteString = \x xs -> fromIntegral <$> LBS.elemIndex x xs
"cons elemIndex bslazy eta"
    forall x xs.
    elemIndex @LBS.ByteString x xs = fromIntegral <$> (LBS.elemIndex x xs)
#-}


{-# inline [1] elemIndices #-}
-- elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices :: (AsEmpty s, Cons s s a a, Eq a) => a -> s -> [Int]
elemIndices x = findIndices (x==)

{-# rules
"cons elemIndices bs"
    elemIndices @BS.ByteString = BS.elemIndices
"cons elemIndices bs eta"
    forall x xs.
    elemIndices @BS.ByteString x xs = BS.elemIndices x xs

"cons elemIndices bslazy"
    elemIndices @LBS.ByteString = \x xs -> fromIntegral <$> LBS.elemIndices x xs
"cons elemIndices bslazy eta"
    forall x xs.
    elemIndices @LBS.ByteString x xs = fromIntegral <$> (LBS.elemIndices x xs)
#-}



{-# inline [2] findIndex #-}
-- findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> Maybe Int
findIndex p = listToMaybe . findIndices p

{-# rules
"cons findIndex text"
    findIndex @Text @Char = Data.Text.findIndex
"cons findIndex text eta"
    forall p xs.
    findIndex @Text @Char p xs = Data.Text.findIndex p xs

"cons findIndex vector"
    findIndex @(Vector _) = Data.Vector.findIndex
"cons findIndex vector eta"
    forall p xs.
    findIndex @(Vector _) p xs = Data.Vector.findIndex p xs

"cons findIndex bs"
    findIndex @BS.ByteString = BS.findIndex
"cons findIndex bs eta"
    forall p xs.
    findIndex @BS.ByteString p xs = BS.findIndex p xs

"cons findIndex bslazy"
    findIndex @LBS.ByteString = \p xs -> fromIntegral <$> LBS.findIndex p xs
"cons findIndex bslazy eta"
    forall p xs.
    findIndex @LBS.ByteString p xs = fromIntegral <$> (LBS.findIndex p xs)
#-}


{-# inline [1] findIndices #-}
-- findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> [Int]
-- findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
-- Efficient definition, adapted from Data.Sequence
findIndices p ls = build $ \c n ->
  let go x r k | p x       = I# k `c` r (k +# 1#)
               | otherwise = r (k +# 1#)
  in foldr go (\_ -> n) ls 0#

{-# rules
"cons findIndices bs"
    findIndices @BS.ByteString = BS.findIndices
"cons findIndices bs eta"
    forall p xs.
    findIndices @BS.ByteString p xs = BS.findIndices p xs

"cons findIndices bslazy"
    findIndices @LBS.ByteString = \p xs -> fromIntegral <$> LBS.findIndices p xs
"cons findIndices bslazy eta"
    forall p xs.
    findIndices @LBS.ByteString p xs = fromIntegral <$> (LBS.findIndices p xs)
#-}
