{-
== Sublists (Extracting sublists) ==
+ take
+ drop
+ splitAt
+ takeWhile
+ dropWhile
+ dropWhileEnd
+ span
+ break
+ stripPrefix
+ group
+ groupBy
+ inits
+ tails
-}

{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.ExtractingSublists
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , take
  , drop
  , splitAt
  , takeWhile
  , dropWhile
  , dropWhileEnd
  , span
  , break
  , stripPrefix
  , group
  , groupBy
  , inits
  , tails
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Bool (Bool(..), (&&), not, otherwise)
import Data.Char (Char)
import Data.Int (Int)
import Data.Eq (Eq(..))
import Data.Function ((.))
import Data.Maybe (Maybe(..))
import Data.Ord ((<=), (<))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Num ((-))
import GHC.Real (Integral, fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Basic
import Consy.Folds


{- ___ Sublists (Extracting sublists) _______________________________________ -}

{-# inline [1] take #-}
-- take :: Int -> [a] -> [a]
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

{-# inline [0] flipSeqTake #-}
flipSeqTake :: a -> Int -> a
flipSeqTake x !_ = x

{-# inline [0] takeFB #-}
takeFB :: (a -> b -> b) -> b -> a -> (Int -> b) -> Int -> b
takeFB c n x xs =
  \m -> case m of
    1 -> x `c` n
    _ -> x `c` xs (m - 1)

{-# rules
"cons take" [~1]
    forall n xs.
    take n xs =
      build (\c nil -> if 0 < n
                       then foldr (takeFB c nil) (flipSeqTake nil) xs n
                       else nil)

"cons unsafeTakeList" [1]
    forall n xs.
    foldr (takeFB (:) []) (flipSeqTake []) xs n = unsafeTake n xs

"cons take text"
    take @Text = Data.Text.take
"cons take text eta"
    forall n xs.
    take @Text n xs = Data.Text.take n xs

"cons take ltext"
    take @Data.Text.Lazy.Text = \n -> Data.Text.Lazy.take (fromIntegral n)
"cons take ltext eta"
    forall n xs.
    take @Data.Text.Lazy.Text n xs = Data.Text.Lazy.take (fromIntegral n) xs

"cons take vector"
    take @(Vector _) = Data.Vector.take
"cons take vector eta"
    forall n xs.
    take @(Vector _) n xs = Data.Vector.take n xs

"cons take bs"
    take @BS.ByteString = BS.take
"cons take bs eta"
    forall n xs.
    take @BS.ByteString n xs = BS.take n xs

"cons take bslazy"
    take @LBS.ByteString = LBS.take . fromIntegral
"cons take bslazy eta"
    forall n xs. take @LBS.ByteString n xs = LBS.take (fromIntegral n) xs

"cons take seq"
    take @(Seq _) = Data.Sequence.take
"cons take seq eta"
    forall n xs.
    take @(Seq _) n xs = Data.Sequence.take n xs
#-}


{-# inline [1] drop #-}
-- drop :: Int -> [a] -> [a]
drop :: (AsEmpty s, Cons s s a a) => Int -> s -> s
drop = go
  where
    go !n s
      | 0 < n =
          case uncons s of
            Nothing -> Empty
            Just (x, xs) -> go (n-1) xs
      | otherwise = Empty

{-# noinline [1] unsafeDrop #-}
unsafeDrop :: (AsEmpty s, Cons s s a a) => Int -> s -> s
unsafeDrop !m s =
  case m of
    1 -> case uncons s of; Just (_, xs) -> xs
    _ ->
      case uncons s of
        Nothing -> Empty
        Just (_, xs) -> unsafeDrop (m - 1) xs

{-# rules
"cons drop text" [~1]
    drop @Text = Data.Text.drop
"cons drop text eta" [~1]
    forall n xs.
    drop @Text n xs = Data.Text.drop n xs

"cons drop ltext" [~1]
    drop @Data.Text.Lazy.Text = Data.Text.Lazy.drop . fromIntegral
"cons drop ltext eta" [~1]
    forall n xs.
    drop @Data.Text.Lazy.Text n xs = Data.Text.Lazy.drop (fromIntegral n) xs

"cons drop vector" [~1]
    drop @(Vector _) = Data.Vector.drop
"cons drop vector eta" [~1]
    forall n xs.
    drop @(Vector _) n xs = Data.Vector.drop n xs

"cons drop bs" [~1]
    drop @BS.ByteString = BS.drop
"cons drop bs eta" [~1]
    forall n xs.
    drop @BS.ByteString n xs = BS.drop n xs

"cons drop bslazy" [~1]
    drop @LBS.ByteString = LBS.drop . fromIntegral
"cons drop bslazy eta" [~1]
    forall n xs. drop @LBS.ByteString n xs = LBS.drop (fromIntegral n) xs

"cons drop seq" [~1]
    drop @(Seq _) = Data.Sequence.drop
"cons drop seq eta" [~1]
    forall n xs.
    drop @(Seq _) n xs = Data.Sequence.drop n xs
#-}


{-# inline [2] splitAt #-}
-- splitAt :: Int -> [a] -> ([a], [a])
splitAt :: (AsEmpty s, Cons s s a a) => Int -> s -> (s, s)
splitAt = \n ls ->
  if n <= 0
  then (Empty, ls)
  else splitAt' n ls
    where
      splitAt' m a =
        case uncons a of
          Nothing -> (Empty, Empty)
          Just (x, xs)
            | 1 <- m -> (cons x Empty, xs)
            | otherwise ->
              let
                (xs', xs'') = splitAt' (m - 1) xs
              in
                (x `cons` xs', xs'')

{-# rules
"cons splitAt text"
    splitAt @Text = Data.Text.splitAt
"cons splitAt text eta"
    forall n xs.
    splitAt @Text n xs = Data.Text.splitAt n xs

"cons splitAt ltext"
    splitAt @Data.Text.Lazy.Text = \n -> Data.Text.Lazy.splitAt (fromIntegral n)
"cons splitAt ltext eta"
    forall n xs.
    splitAt @Data.Text.Lazy.Text n xs = Data.Text.Lazy.splitAt (fromIntegral n) xs

"cons splitAt vector"
    splitAt @(Vector _) = Data.Vector.splitAt
"cons splitAt vector eta"
    forall n xs.
    splitAt @(Vector _) n xs = Data.Vector.splitAt n xs

"cons splitAt bs"
    splitAt @BS.ByteString = BS.splitAt
"cons splitAt bs eta"
    forall n xs.
    splitAt @BS.ByteString n xs = BS.splitAt n xs

"cons splitAt bslazy"
    splitAt @LBS.ByteString = \n -> LBS.splitAt (fromIntegral n)
"cons splitAt bslazy eta"
    forall n xs.
    splitAt @LBS.ByteString n xs = LBS.splitAt (fromIntegral n) xs

"cons splitAt seq"
    splitAt @(Seq _) = Data.Sequence.splitAt
"cons splitAt seq eta"
    forall n xs.
    splitAt @(Seq _) n xs = Data.Sequence.splitAt n xs
#-}


{-# noinline [1] takeWhile #-}
-- takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> s
takeWhile = \p -> go p
  where
    go p s =
      case uncons s of
        Empty -> Empty
        Just (x, xs) -> if p x
                        then x `cons` (go p xs)
                        else Empty

{-# inline [0] takeWhileFB #-}
takeWhileFB :: (a -> Bool) -> (a -> b -> b) -> b -> a -> b -> b
takeWhileFB p c n = \x r -> if p x then x `c` r else n

{-# rules
"cons takeWhile" [~1]
    forall p xs.
    takeWhile p xs = build (\c n -> foldr (takeWhileFB p c n) n xs)
"cons takeWhileList" [1]
    forall p.
    foldr (takeWhileFB p (:) []) [] = takeWhile p
"cons takeWhileFB"
    forall c n p q.
    takeWhileFB q (takeWhileFB p c n) n = takeWhileFB (\x -> q x && p x) c n

"cons takeWhile text"
    takeWhile @Text @Char = Data.Text.takeWhile
"cons takeWhile text eta"
    forall p xs.
    takeWhile @Text @Char p xs = Data.Text.takeWhile p xs

"cons takeWhile ltext"
    takeWhile @Data.Text.Lazy.Text @Char = Data.Text.Lazy.takeWhile
"cons takeWhile ltext eta"
    forall p xs.
    takeWhile @Data.Text.Lazy.Text @Char p xs = Data.Text.Lazy.takeWhile p xs

"cons takeWhile vector" takeWhile @(Vector _) =
    Data.Vector.takeWhile
"cons takeWhile vector eta"
    forall p xs.
    takeWhile @(Vector _) p xs = Data.Vector.takeWhile p xs

"cons takeWhile bs"
    takeWhile @BS.ByteString = BS.takeWhile
"cons takeWhile bs eta"
    forall p xs.
    takeWhile @BS.ByteString p xs = BS.takeWhile p xs

"cons takeWhile bslazy"
    takeWhile @LBS.ByteString = LBS.takeWhile
"cons takeWhile bslazy eta"
    forall p xs.
    takeWhile @LBS.ByteString p xs = LBS.takeWhile p xs
#-}


{-# noinline [1] dropWhile #-}
-- dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> s
dropWhile = \p -> go p
  where
    go p s =
      case uncons s of
        Empty -> Empty
        Just (x, xs) -> if p x
                        then go p xs
                        else xs

{-# rules
"cons dropWhile text"
    dropWhile @Text @Char = Data.Text.dropWhile
"cons dropWhile text eta"
    forall p xs.
    dropWhile @Text @Char p xs = Data.Text.dropWhile p xs

"cons dropWhile ltext"
    dropWhile @Data.Text.Lazy.Text @Char = Data.Text.Lazy.dropWhile
"cons dropWhile ltext eta"
    forall p xs.
    dropWhile @Data.Text.Lazy.Text @Char p xs = Data.Text.Lazy.dropWhile p xs

"cons dropWhile vector" dropWhile @(Vector _) =
    Data.Vector.dropWhile
"cons dropWhile vector eta"
    forall p xs.
    dropWhile @(Vector _) p xs = Data.Vector.dropWhile p xs

"cons dropWhile bs"
    dropWhile @BS.ByteString = BS.dropWhile
"cons dropWhile bs eta"
    forall p xs.
    dropWhile @BS.ByteString p xs = BS.dropWhile p xs

"cons dropWhile bslazy"
    dropWhile @LBS.ByteString = LBS.dropWhile
"cons dropWhile bslazy eta"
    forall p xs.
    dropWhile @LBS.ByteString p xs = LBS.dropWhile p xs
#-}


{-# noinline [1] dropWhileEnd #-}
-- dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> s
dropWhileEnd = \p ->
  foldr
    (\x xs -> if p x && null xs
              then Empty
              else x `cons` xs)
    Empty

{-# rules
"cons dropWhileEnd text"
    dropWhileEnd @Text @Char = Data.Text.dropWhileEnd
"cons dropWhileEnd text eta"
    forall p xs.
    dropWhileEnd @Text @Char p xs = Data.Text.dropWhileEnd p xs

"cons dropWhileEnd ltext"
    dropWhileEnd @Data.Text.Lazy.Text @Char = Data.Text.Lazy.dropWhileEnd
"cons dropWhileEnd ltext eta"
    forall p xs.
    dropWhileEnd @Data.Text.Lazy.Text @Char p xs = Data.Text.Lazy.dropWhileEnd p xs
#-}


{-# noinline [1] span #-}
-- span :: (a -> Bool) -> [a] -> ([a], [a])
span :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> (s, s)
span = \p -> go p
  where
    go p s =
      case uncons s of
        Empty -> (s, s)
        Just (x, xs) -> if p x
                        then let (ys,zs) = go p xs in (x `cons` ys,zs)
                        else (Empty, s)

{-# rules
"cons span text"
    span @Text = Data.Text.span
"cons span text eta"
    forall p xs.
    span @Text p xs = Data.Text.span p xs

"cons span ltext"
    span @Data.Text.Lazy.Text = Data.Text.Lazy.span
"cons span ltext eta"
    forall p xs.
    span @Data.Text.Lazy.Text p xs = Data.Text.Lazy.span p xs

"cons span vector"
    span @(Vector _) = Data.Vector.span
"cons span vector eta"
    forall p xs.
    span @(Vector _) p xs = Data.Vector.span p xs

"cons span bs"
    span @BS.ByteString = BS.span
"cons span bs eta"
    forall p xs.
    span @BS.ByteString p xs = BS.span p xs

"cons span bslazy"
    span @LBS.ByteString = LBS.span
"cons span bslazy eta"
    forall p xs.
    span @LBS.ByteString p xs = LBS.span p xs
#-}


{-# noinline [1] break #-}
-- break :: (a -> Bool) -> [a] -> ([a], [a])
break :: (AsEmpty s, Cons s s a a) => (a -> Bool) -> s -> (s, s)
break p = span (not . p)

{-# rules
"cons break text"
    break @Text = Data.Text.break
"cons break text eta"
    forall p xs.
    break @Text p xs = Data.Text.break p xs

"cons break ltext"
    break @Data.Text.Lazy.Text = Data.Text.Lazy.break
"cons break ltext eta"
    forall p xs.
    break @Data.Text.Lazy.Text p xs = Data.Text.Lazy.break p xs

"cons break vector"
    break @(Vector _) = Data.Vector.break
"cons break vector eta"
    forall p xs.
    break @(Vector _) p xs = Data.Vector.break p xs

"cons break bs"
    break @BS.ByteString = BS.break
"cons break bs eta"
    forall p xs.
    break @BS.ByteString p xs = BS.break p xs

"cons break bslazy"
    break @LBS.ByteString = LBS.break
"cons break bslazy eta"
    forall p xs.
    break @LBS.ByteString p xs = LBS.break p xs
#-}


{-# inline [2] stripPrefix #-}
-- stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix :: (Cons s s a a, Eq a) => s -> s -> Maybe s
stripPrefix = \s -> go s
  where
    go s ys =
      case uncons s of
        Nothing -> Just ys
        Just (x, xs)
          | Just (y', ys') <- uncons ys
          ,  x == y' -> go xs ys'
        _ -> Nothing

{-# rules
"cons stripPrefix text"
    stripPrefix @Text = Data.Text.stripPrefix
"cons stripPrefix text eta"
    forall xs ys.
    stripPrefix @Text xs ys = Data.Text.stripPrefix xs ys

"cons stripPrefix ltext"
    stripPrefix @Data.Text.Lazy.Text = Data.Text.Lazy.stripPrefix
"cons stripPrefix ltext eta"
    forall xs ys.
    stripPrefix @Data.Text.Lazy.Text xs ys = Data.Text.Lazy.stripPrefix xs ys
#-}


{-# inline [2] group #-}
-- group :: Eq a => [a] -> [[a]]
group :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t s s, Eq a) => s -> t
group = groupBy (==)

{-# rules
"cons group text"
    group @Text = Data.Text.group
"cons group text eta"
    forall xs.
    group @Text xs = Data.Text.group xs

"cons group ltext"
    group @Data.Text.Lazy.Text = Data.Text.Lazy.group
"cons group ltext eta"
    forall xs.
    group @Data.Text.Lazy.Text xs = Data.Text.Lazy.group xs

"cons group bs"
    group @BS.ByteString = BS.group
"cons group bs eta"
    forall xs.
    group @BS.ByteString xs = BS.group xs

"cons group bslazy"
    group @LBS.ByteString = LBS.group
"cons group bslazy eta"
    forall xs.
    group @LBS.ByteString xs = LBS.group xs
#-}


{-# inline [2] groupBy #-}
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t s s) => (a -> a -> Bool) -> s -> t
groupBy = \p -> go p
  where
    go p s =
      case uncons s of
        Empty -> Empty
        Just (x, xs) -> (x `cons` ys) `cons` (go p zs)
                          where (ys, zs) = span (p x) xs

{-# rules
"cons groupBy text"
    groupBy @Text = Data.Text.groupBy
"cons groupBy text eta"
    forall p xs.
    groupBy @Text p xs = Data.Text.groupBy p xs

"cons groupBy ltext"
    groupBy @Data.Text.Lazy.Text = Data.Text.Lazy.groupBy
"cons groupBy ltext eta"
    forall p xs.
    groupBy @Data.Text.Lazy.Text p xs = Data.Text.Lazy.groupBy p xs

"cons groupBy bs"
    groupBy @BS.ByteString = BS.groupBy
"cons groupBy bs eta"
    forall p xs.
    groupBy @BS.ByteString p xs = BS.groupBy p xs

"cons groupBy bslazy"
    groupBy @LBS.ByteString = LBS.groupBy
"cons groupBy bslazy eta"
    forall p xs.
    groupBy @LBS.ByteString p xs = LBS.groupBy p xs
#-}


{-# noinline inits #-}
-- inits :: [a] -> [[a]]
inits :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t s s) => s -> t
-- inits = map toListSB . scanl' snocSB emptySB
inits lst = build (\c n ->
  let initsGo hs xs = hs `c` case uncons xs of
                            Empty -> n
                            Just (x',xs') -> initsGo (hs `append` (x' `cons` Empty)) xs'
  in initsGo Empty lst)
{-
  myinits :: [a] -> [[a]]
  myinits lst = build ( \c n ->
    let initsGo hs xs = hs `c` case xs of
                              [] -> n
                              (x':xs') -> initsGo (hs ++ [x']) xs'
    in initsGo [] lst )

-}

{-# rules
"cons inits text"
    inits @Text = Data.Text.inits
"cons inits text eta"
    forall xs.
    inits @Text xs = Data.Text.inits xs

"cons inits ltext"
    inits @Data.Text.Lazy.Text = Data.Text.Lazy.inits
"cons inits ltext eta"
    forall xs.
    inits @Data.Text.Lazy.Text xs = Data.Text.Lazy.inits xs

"cons inits bs"
    inits @BS.ByteString = BS.inits
"cons inits bs eta"
    forall xs.
    inits @BS.ByteString xs = BS.inits xs

"cons inits bslazy"
    inits @LBS.ByteString = LBS.inits
"cons inits bslazy eta"
    forall xs.
    inits @LBS.ByteString xs = LBS.inits xs

"cons inits seq"
    inits @(Seq _) = Data.Sequence.inits
"cons inits seq eta"
    forall xs.
    inits @(Seq _) xs = Data.Sequence.inits xs
#-}


{-# inlineable [2] tails #-}
-- tails :: [a] -> [[a]]
tails :: (AsEmpty s, AsEmpty t, Cons s s a a, Cons t t s s) => s -> t
tails lst =  build (\c n ->
  let tailsGo xs = xs `c` case uncons xs of
                            Empty -> n
                            Just (_, xs') -> tailsGo xs'
  in tailsGo lst)

{-# rules
"cons tails text"
    tails @Text = Data.Text.tails
"cons tails text eta"
    forall xs.
    tails @Text xs = Data.Text.tails xs

"cons tails ltext"
    tails @Data.Text.Lazy.Text = Data.Text.Lazy.tails
"cons tails ltext eta"
    forall xs.
    tails @Data.Text.Lazy.Text xs = Data.Text.Lazy.tails xs

"cons tails bs"
    tails @BS.ByteString = BS.tails
"cons tails bs eta"
    forall xs.
    tails @BS.ByteString xs = BS.tails xs

"cons tails bslazy"
    tails @LBS.ByteString = LBS.tails
"cons tails bslazy eta"
    forall xs.
    tails @LBS.ByteString xs = LBS.tails xs

"cons tails seq"
    tails @(Seq _) = Data.Sequence.tails
"cons tails seq eta"
    forall xs.
    tails @(Seq _) xs = Data.Sequence.tails xs
#-}
