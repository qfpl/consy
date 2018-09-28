{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin -ddump-to-file -ddump-simpl #-}
module InspectionTests where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), Bool, otherwise)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Ord ((<), (>))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.Num (Num, Integer, (+), (-))
import GHC.Real (fromIntegral)
import Test.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Internal.Fusion
import Consy

consFilter, listFilter :: (a -> Bool) -> [a] -> [a]
consFilter = filter
listFilter p = go
  where
    go [] = []
    go (x : xs)
      | p x = x : go xs | otherwise = go xs

inspect ('consFilter === 'listFilter)


consFilterZipList, zipListFilter :: (a -> Bool) -> ZipList a -> ZipList a
consFilterZipList = filter
zipListFilter p = coerce (listFilter p)

inspect ('consFilterZipList ==- 'zipListFilter)


{-# noinline consFilterSeq #-}
{-# noinline seqFilter #-}
consFilterSeq, seqFilter :: (a -> Bool) -> Seq a -> Seq a
consFilterSeq = filter
seqFilter = Data.Sequence.filter

inspect ('consFilterSeq === 'seqFilter)


{-# noinline consFoldl'Seq #-}
{-# noinline seqFoldl' #-}
consFoldl'Seq, seqFoldl' :: (b -> a -> b) -> b -> Seq a -> b
consFoldl'Seq = foldl'
seqFoldl' = Data.Foldable.foldl'

inspect ('consFoldl'Seq === 'seqFoldl')


consTakeText, textTake :: Int -> Text -> Text
consTakeText = take
textTake = Data.Text.take

inspect ('consTakeText === 'textTake)


consTakeText', textTake' :: Text -> Text
consTakeText' = take 10
textTake' = Data.Text.take 10

inspect ('consTakeText' === 'textTake')


consTakeText'', textTake'' :: Text
consTakeText'' = take 10 (pack "bbbb")
textTake'' = Data.Text.take 10 (pack "bbbb")

inspect ('consTakeText'' === 'textTake'')


consTakeList, listTake :: Int -> [a] -> [a]
consTakeList = take
listTake = go
  where
    go !n s
      | 0 < n =
          case s of
            [] -> []
            x : xs -> x : go (n-1) xs
      | otherwise = []

inspect ('consTakeList === 'listTake)


consMapTakeList, listMapTake :: (a -> a) -> Int -> [a] -> [a]
consMapTakeList f n = map f . take n
listMapTake f !n s
  | 0 < n = go s n
  | otherwise = []
  where
    go s !n =
      case s of
        [] -> []
        x : xs ->
          case n of
            1 -> [f x]
            -- It's flipped because of the take + unsafeTakeList rules
            _ -> f x : go xs (n-1)

inspect ('consMapTakeList === 'listMapTake)

consTakeZipList, zipListTake :: Int -> ZipList a -> ZipList a
consTakeZipList n = take n
zipListTake n = ZipList . listTake n . getZipList

inspect ('consTakeZipList ==- 'zipListTake)


consFoldl'Text, textFoldl' :: (a -> Char -> a) -> a -> Text -> a
consFoldl'Text = foldl'
textFoldl' = Data.Text.foldl'

inspect ('consFoldl'Text === 'textFoldl')


consFoldl'Text2, textFoldl'2 :: Int -> Text -> Int
consFoldl'Text2 = foldl' (\a _ -> a + 1)
textFoldl'2 = Data.Text.foldl' (\a _ -> a + 1)

inspect ('consFoldl'Text2 === 'textFoldl'2)


consFoldl'Text4, textFoldl'4 :: Int
consFoldl'Text4 = foldl' (\a _ -> a + 1) 0 (pack "aaaa")
textFoldl'4 = Data.Text.foldl' (\a _ -> a + 1) 0 (pack "aaaa")

inspect ('consFoldl'Text4 === 'textFoldl'4)


consFilterText, textFilter :: (Char -> Bool) -> Text -> Text
consFilterText = filter
textFilter = Data.Text.filter

inspect ('consFilterText === 'textFilter)


consFilterText', textFilter' :: Text -> Text
consFilterText' = filter (== 'a')
textFilter' = Data.Text.filter (== 'a')

inspect ('consFilterText' === 'textFilter')


consFilterText'', textFilter'' :: Text
consFilterText'' = filter (== 'a') (pack "bbbb")
textFilter'' = Data.Text.filter (== 'a') (pack "bbbb")

inspect ('consFilterText'' === 'textFilter'')


consFilterMapText, textFilterMap :: Text -> Text
consFilterMapText = map succ . filter (== 'a')
textFilterMap = Data.Text.map succ . Data.Text.filter (== 'a')

inspect ('consFilterMapText === 'textFilterMap)


consMapFilter, listMapFilter :: Num a => (a -> Bool) -> [a] -> [a]
consMapFilter p = filter p . map (+1)
listMapFilter p = Data.List.filter p . Data.List.map (+1)

inspect ('consMapFilter === 'listMapFilter)


consFoldrListLength, listFoldrLength :: [a] -> Int
consFoldrListLength = foldr (\_ -> (+1)) 0
listFoldrLength = Data.List.foldr (\_ -> (+1)) 0

inspect ('consFoldrListLength === 'listFoldrLength)


consMapFoldrTextLength, textMapFoldrLength :: (Char -> Char) -> Text -> Int
consMapFoldrTextLength f = foldr (\_ -> (+1)) 0 . map f
textMapFoldrLength f = Data.Text.foldr (\_ -> (+1)) 0 . Data.Text.map f

-- not the same
-- inspect ('consMapFoldrTextLength === 'textMapFoldrLength)


consFilterPFilterQ, consFilterPQ, listFilterPQ
  :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
consFilterPFilterQ p q xs = filter p (filter q xs)
consFilterPQ p q xs = filter (\x -> q x && p x) xs
listFilterPQ p q = go
  where
    go [] = []
    go (x : xs)
      | q x && p x = x : go xs
      | otherwise = go xs

inspect ('consFilterPFilterQ === 'consFilterPQ)
inspect ('consFilterPQ === 'listFilterPQ)


consFilterPFilterQZipList, consFilterPQZipList, zipListFilterPQ
  :: (a -> Bool) -> (a -> Bool) -> ZipList a -> ZipList a
consFilterPFilterQZipList p q = coerce (consFilterPFilterQ p q)
consFilterPQZipList p q = coerce (consFilterPQ p q)
zipListFilterPQ p q = coerce (listFilterPQ p q)

inspect ('consFilterPFilterQZipList === 'consFilterPQZipList)
inspect ('consFilterPQZipList === 'zipListFilterPQ)


consReplicate, listReplicate :: Int -> a -> [a]
consReplicate = replicate
listReplicate n a
  | 0 < n = go n
  | otherwise = []
  where
    go 1 = [a]
    go n = a : go (n-1)

inspect ('consReplicate === 'listReplicate)


consReplicate', listReplicate' :: [Char]
consReplicate' = replicate 100 'a'
listReplicate' = Data.List.replicate 100 'a'

inspect ('consReplicate' === 'listReplicate')


consReplicateMap, listReplicateMap :: [Int]
consReplicateMap = map (+10) (replicate 100 10)
listReplicateMap = Data.List.map (+10) (Data.List.replicate 100 10)

inspect ('consReplicateMap === 'listReplicateMap)


consReplicateMap', listReplicateMap' :: Int -> [Int]
consReplicateMap' n = map (+10) (replicate n 10)
listReplicateMap' n = Data.List.map (+10) (Data.List.replicate n 10)

inspect ('consReplicateMap' === 'listReplicateMap')


{-# noinline consReplicateText #-}
{-# noinline textReplicate #-}
consReplicateText, textReplicate :: Int -> Char -> Text
consReplicateText = replicate
textReplicate n = Data.Text.replicate n . Data.Text.singleton

inspect ('consReplicateText === 'textReplicate)


consListLength, listLength :: [a] -> Int
consListLength = length
listLength = go 0
  where
    go !n s =
      case s of
        [] -> n
        _ : xs -> go (n + 1) xs

inspect ('consListLength === 'listLength)


{-# noinline consLength #-}
{-# noinline consFoldrLength #-}
{-# noinline textFoldrLength #-}
{-# noinline textLength #-}
consLength, consFoldrLength, textFoldrLength, textLength :: Text -> Int
consLength = length
textLength = Data.Text.length
consFoldrLength = foldr (\_ -> (+1)) 0
textFoldrLength = Data.Text.foldr (\_ -> (+1)) 0

inspect ('consLength === 'textLength)

{-# noinline consFoldl'Length #-}
{-# noinline textFoldl'Length #-}
consFoldl'Length, textFoldl'Length :: Text -> Int
consFoldl'Length = foldl' (\b _ -> b + 1) 0
textFoldl'Length = Data.Text.foldl' (\b _ -> b + 1) 0

inspect ('consFoldl'Length === 'textFoldl'Length)

consFoldrLengthBS, bsFoldrLength :: Data.ByteString.ByteString -> Int
consFoldrLengthBS = foldr (\_ -> (+1)) 0
bsFoldrLength = Data.ByteString.foldr (\_ -> (+1)) 0

consFoldrLengthLBS, lbsFoldrLength :: Data.ByteString.Lazy.ByteString -> Int
consFoldrLengthLBS = foldr (\_ -> (+1)) 0
lbsFoldrLength = Data.ByteString.Lazy.foldr (\_ -> (+1)) 0

inspect ('consFoldrLengthLBS === 'lbsFoldrLength)


consFoldrLengthLText, ltextFoldrLength :: Data.Text.Lazy.Text -> Int
consFoldrLengthLText = foldr (\_ -> (+1)) 0
ltextFoldrLength = Data.Text.Lazy.foldr (\_ -> (+1)) 0

-- inspect ('consFoldrLengthLBS === 'lbsFoldrLength)


consFoldrLengthSeq, seqFoldrLength :: Data.Sequence.Seq a -> Int
consFoldrLengthSeq = foldr (\_ -> (+1)) 0
seqFoldrLength = Data.Foldable.foldr (\_ -> (+1)) 0

inspect ('consFoldrLengthSeq === 'seqFoldrLength)


consLengthSeq, seqLength :: Data.Sequence.Seq a -> Int
consLengthSeq = length
seqLength = Data.Sequence.length

inspect ('consLengthSeq === 'seqLength)


{-# noinline consMapSeq #-}
{-# noinline seqMap #-}
consMapSeq, seqMap :: (a -> a) -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consMapSeq = map
seqMap = Data.Functor.fmap

inspect ('consMapSeq === 'seqMap)


{-# noinline consTakeSeq #-}
{-# noinline seqTake #-}
consTakeSeq, seqTake :: Int -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consTakeSeq = take
seqTake = Data.Sequence.take

inspect ('consTakeSeq === 'seqTake)


-- for some reason eta-reducing prevents it from inlining
consZipWithList, listZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
consZipWithList f = zipWith f
listZipWith f = go
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys) = f x y : go xs ys

inspect ('consZipWithList === 'listZipWith)


consAppendList, listAppend :: [a] -> [a] -> [a]
consAppendList = append
listAppend [] ys = ys
listAppend (x:xs) ys = x : listAppend xs ys

inspect ('consAppendList === 'listAppend)


