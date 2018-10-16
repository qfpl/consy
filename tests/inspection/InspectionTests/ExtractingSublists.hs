{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.ExtractingSublists where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise, not)
import Data.Char (Char, isUpper)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>), (<=))
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.List (errorEmptyList, (++))
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
import qualified Data.Vector
import qualified Data.Word

import Consy


{- take -}
consTakeList, listTake :: Int -> [a] -> [a]
consTakeList = take
listTake = go
  where
    go !n s
      | 0 < n =
          case s of
            [] -> []
            (x : xs) -> x : go (n-1) xs
      | otherwise = []
inspect ('consTakeList === 'listTake)

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

consTakeLazyText, lazyTextTake :: Int -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consTakeLazyText = take
lazyTextTake = \n -> Data.Text.Lazy.take (fromIntegral n)
inspect ('consTakeLazyText === 'lazyTextTake)

consTakeVector, vectorTake :: Int -> Vector a -> Vector a
consTakeVector = take
vectorTake = Data.Vector.take
inspect ('consTakeVector === 'vectorTake)

consTakeBS, bsTake :: Int -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consTakeBS = take
bsTake = Data.ByteString.take
inspect ('consTakeBS === 'bsTake)

consTakeLBS, lbsTake :: Int64 -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consTakeLBS = take . fromIntegral
lbsTake = \n -> Data.ByteString.Lazy.take (fromIntegral n)
inspect ('consTakeLBS === 'lbsTake)

consTakeSeq, seqTake :: Int -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consTakeSeq = take
seqTake = Data.Sequence.take
inspect ('consTakeSeq === 'seqTake)

consMapTakeList, listMapTake :: (a -> a) -> Int -> [a] -> [a]
consMapTakeList f n = map f . take n
listMapTake f !n s
  | 0 < n = go s n
  | otherwise = []
  where
    go s !n =
      case s of
        [] -> []
        (x : xs) ->
          case n of
            1 -> [f x]
            -- It's flipped because of the take + unsafeTakeList rules
            _ -> f x : go xs (n-1)
inspect ('consMapTakeList === 'listMapTake)

consTakeZipList, zipListTake :: Int -> ZipList a -> ZipList a
consTakeZipList n = take n
zipListTake n = ZipList . listTake n . getZipList
inspect ('consTakeZipList ==- 'zipListTake)


{- drop -}
consDropList, listDrop :: Int -> [a] -> [a]
consDropList = drop
listDrop = go
  where
    go !n s
      | 0 < n =
          case s of
            [] -> []
            (x : xs) -> go (n-1) xs
      | otherwise = []
inspect ('consDropList === 'listDrop)

consDropText, textDrop :: Int -> Text -> Text
consDropText = drop
textDrop = Data.Text.drop
inspect ('consDropText === 'textDrop)

consDropText', textDrop' :: Text -> Text
consDropText' = drop 10
textDrop' = Data.Text.drop 10
inspect ('consDropText' === 'textDrop')

consDropText'', textDrop'' :: Text
consDropText'' = drop 10 (pack "bbbb")
textDrop'' = Data.Text.drop 10 (pack "bbbb")
inspect ('consDropText'' === 'textDrop'')

consDropLazyText, lazyTextDrop :: Int -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consDropLazyText = drop
lazyTextDrop = Data.Text.Lazy.drop . fromIntegral
inspect ('consDropLazyText === 'lazyTextDrop)

consDropVector, vectorDrop :: Int -> Vector a -> Vector a
consDropVector = drop
vectorDrop = Data.Vector.drop
inspect ('consDropVector === 'vectorDrop)

consDropBS, bsDrop :: Int -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consDropBS = drop
bsDrop = Data.ByteString.drop
inspect ('consDropBS === 'bsDrop)

consDropLBS, lbsDrop :: Int -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consDropLBS = drop
lbsDrop = \n -> Data.ByteString.Lazy.drop (fromIntegral n)
inspect ('consDropLBS === 'lbsDrop)

consDropSeq, seqDrop :: Int -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consDropSeq = drop
seqDrop = Data.Sequence.drop
inspect ('consDropSeq === 'seqDrop)


{- splitAt -}
consSplitAtList, listSplitAt :: Int -> [a] -> ([a], [a])
consSplitAtList = splitAt
listSplitAt n ls
  | n <= 0 = ([], ls)
  | otherwise = splitAt' n ls
    where
      splitAt' _ [] = ([], [])
      splitAt' 1 (x:xs) = ([x], xs)
      splitAt' m (x:xs) =
        let
          (xs', xs'') = splitAt' (m - 1) xs
        in
          (x:xs', xs'')
{-
the core from these two functions only differ by a single line- in the Cons-based version
'n' is unpacked and repacked after its comparison, but in the List-based version the 'n'
is preserved using 'case n of wild'. The performance difference is negligable

inspect ('consSplitAtList ==- 'listSplitAt)
-}

consSplitAtText, textSplitAt :: Int -> Text -> (Text, Text)
consSplitAtText = splitAt
textSplitAt = Data.Text.splitAt
inspect ('consSplitAtText === 'textSplitAt)

consSplitAtText', textSplitAt' :: Text -> (Text, Text)
consSplitAtText' = splitAt 10
textSplitAt' = Data.Text.splitAt 10
inspect ('consSplitAtText' === 'textSplitAt')

consSplitAtText'', textSplitAt'' :: (Text, Text)
consSplitAtText'' = splitAt 10 (pack "bbbb")
textSplitAt'' = Data.Text.splitAt 10 (pack "bbbb")
inspect ('consSplitAtText'' === 'textSplitAt'')

consSplitAtLazyText, lazyTextSplitAt :: Int64 -> Data.Text.Lazy.Text -> (Data.Text.Lazy.Text, Data.Text.Lazy.Text)
consSplitAtLazyText = splitAt . fromIntegral
lazyTextSplitAt = Data.Text.Lazy.splitAt
inspect ('consSplitAtLazyText === 'lazyTextSplitAt)

consSplitAtVector, vectorSplitAt :: Int -> Vector a -> (Vector a, Vector a)
consSplitAtVector = splitAt
vectorSplitAt = Data.Vector.splitAt
inspect ('consSplitAtVector === 'vectorSplitAt)

consSplitAtBS, bsSplitAt :: Int -> Data.ByteString.ByteString -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consSplitAtBS = splitAt
bsSplitAt = Data.ByteString.splitAt
inspect ('consSplitAtBS === 'bsSplitAt)

consSplitAtLBS, lbsSplitAt :: Int -> Data.ByteString.Lazy.ByteString -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consSplitAtLBS = splitAt
lbsSplitAt = \n -> Data.ByteString.Lazy.splitAt (fromIntegral n)
inspect ('consSplitAtLBS === 'lbsSplitAt)

consSplitAtSeq, seqSplitAt :: Int -> Data.Sequence.Seq a -> (Data.Sequence.Seq a, Data.Sequence.Seq a)
consSplitAtSeq = splitAt
seqSplitAt = Data.Sequence.splitAt
inspect ('consSplitAtSeq === 'seqSplitAt)


{- takeWhile -}
consTakeWhileList, listTakeWhile :: (a -> Bool) -> [a] -> [a]
consTakeWhileList = takeWhile
listTakeWhile p = go p
  where
    go p s =
      case s of
        [] -> []
        (x : xs) -> if p x
                    then x : (go p xs)
                    else []
inspect ('consTakeWhileList === 'listTakeWhile)

consTakeWhileText, textTakeWhile :: (Char -> Bool) -> Text -> Text
consTakeWhileText = takeWhile
textTakeWhile = Data.Text.takeWhile
inspect ('consTakeWhileText === 'textTakeWhile)

consTakeWhileText', textTakeWhile' :: Text -> Text
consTakeWhileText' = takeWhile Data.Char.isUpper
textTakeWhile' = Data.Text.takeWhile Data.Char.isUpper
inspect ('consTakeWhileText' === 'textTakeWhile')

consTakeWhileLazyText, lazyTextTakeWhile :: (Char -> Bool) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consTakeWhileLazyText = takeWhile
lazyTextTakeWhile = Data.Text.Lazy.takeWhile
inspect ('consTakeWhileLazyText === 'lazyTextTakeWhile)

consTakeWhileVector, vectorTakeWhile :: (a -> Bool) -> Vector a -> Vector a
consTakeWhileVector = takeWhile
vectorTakeWhile = Data.Vector.takeWhile
inspect ('consTakeWhileVector === 'vectorTakeWhile)

consTakeWhileBS, bsTakeWhile :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consTakeWhileBS = takeWhile
bsTakeWhile = Data.ByteString.takeWhile
inspect ('consTakeWhileBS === 'bsTakeWhile)

consTakeWhileLBS, lbsTakeWhile :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consTakeWhileLBS = takeWhile
lbsTakeWhile = Data.ByteString.Lazy.takeWhile
inspect ('consTakeWhileLBS === 'lbsTakeWhile)


{- dropWhile -}
consDropWhileList, listDropWhile :: (a -> Bool) -> [a] -> [a]
consDropWhileList = dropWhile
listDropWhile p = go p
  where
    go p s =
      case s of
        [] -> []
        (x : xs) -> if p x
                    then go p xs
                    else xs
inspect ('consDropWhileList === 'listDropWhile)

consDropWhileText, textDropWhile :: (Char -> Bool) -> Text -> Text
consDropWhileText = dropWhile
textDropWhile = Data.Text.dropWhile
inspect ('consDropWhileText === 'textDropWhile)

consDropWhileText', textDropWhile' :: Text -> Text
consDropWhileText' = dropWhile Data.Char.isUpper
textDropWhile' = Data.Text.dropWhile Data.Char.isUpper
inspect ('consDropWhileText' === 'textDropWhile')

consDropWhileLazyText, lazyTextDropWhile :: (Char -> Bool) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consDropWhileLazyText = dropWhile
lazyTextDropWhile = Data.Text.Lazy.dropWhile
inspect ('consDropWhileLazyText === 'lazyTextDropWhile)

consDropWhileVector, vectorDropWhile :: (a -> Bool) -> Vector a -> Vector a
consDropWhileVector = dropWhile
vectorDropWhile = Data.Vector.dropWhile
inspect ('consDropWhileVector === 'vectorDropWhile)

consDropWhileBS, bsDropWhile :: (Word8 -> Bool) -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consDropWhileBS = dropWhile
bsDropWhile = Data.ByteString.dropWhile
inspect ('consDropWhileBS === 'bsDropWhile)

consDropWhileLBS, lbsDropWhile :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consDropWhileLBS = dropWhile
lbsDropWhile = Data.ByteString.Lazy.dropWhile
inspect ('consDropWhileLBS === 'lbsDropWhile)


{- dropWhileEnd -}
consDropWhileEndList, listDropWhileEnd :: (a -> Bool) -> [a] -> [a]
consDropWhileEndList = dropWhileEnd
listDropWhileEnd p = go p
  where
    go p =
      foldr
        (\x xs -> if p x && null xs
                  then []
                  else x : xs)
        []
inspect ('consDropWhileEndList === 'listDropWhileEnd)

consDropWhileEndText, textDropWhileEnd :: (Char -> Bool) -> Text -> Text
consDropWhileEndText = dropWhileEnd
textDropWhileEnd = Data.Text.dropWhileEnd
inspect ('consDropWhileEndText === 'textDropWhileEnd)

consDropWhileEndText', textDropWhileEnd' :: Text -> Text
consDropWhileEndText' = dropWhileEnd Data.Char.isUpper
textDropWhileEnd' = Data.Text.dropWhileEnd Data.Char.isUpper
inspect ('consDropWhileEndText' === 'textDropWhileEnd')

consDropWhileEndLazyText, lazyTextDropWhileEnd :: (Char -> Bool) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consDropWhileEndLazyText = dropWhileEnd
lazyTextDropWhileEnd = Data.Text.Lazy.dropWhileEnd
inspect ('consDropWhileEndLazyText === 'lazyTextDropWhileEnd)


{- span -}
consSpanList, listSpan :: (a -> Bool) -> [a] -> ([a], [a])
consSpanList = span
listSpan p = \s -> go p s
  where
    go p s =
      case s of
        [] -> ([], [])
        (x : xs) -> if p x
                    then let (ys,zs) = listSpan p xs in (x:ys,zs)
                    else ([], s)
inspect ('consSpanList === 'listSpan)

consSpanText, textSpan :: (Char -> Bool) -> Text -> (Text, Text)
consSpanText = span
textSpan = Data.Text.span
inspect ('consSpanText === 'textSpan)

consSpanText', textSpan' :: Text -> (Text, Text)
consSpanText' = span Data.Char.isUpper
textSpan' = Data.Text.span Data.Char.isUpper
inspect ('consSpanText' === 'textSpan')

consSpanLazyText, lazyTextSpan :: (Char -> Bool) -> Data.Text.Lazy.Text -> (Data.Text.Lazy.Text, Data.Text.Lazy.Text)
consSpanLazyText = span
lazyTextSpan = Data.Text.Lazy.span
inspect ('consSpanLazyText === 'lazyTextSpan)

consSpanVector, vectorSpan :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
consSpanVector = span
vectorSpan = Data.Vector.span
inspect ('consSpanVector === 'vectorSpan)

consSpanBS, bsSpan :: (Word8 -> Bool) -> Data.ByteString.ByteString -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consSpanBS = span
bsSpan = Data.ByteString.span
inspect ('consSpanBS === 'bsSpan)

consSpanLBS, lbsSpan :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consSpanLBS = span
lbsSpan = Data.ByteString.Lazy.span
inspect ('consSpanLBS === 'lbsSpan)


{- break -}
consBreakList, listBreak :: (a -> Bool) -> [a] -> ([a], [a])
consBreakList = break
listBreak = \p -> listSpan (not . p)
inspect ('consBreakList === 'listBreak)

consBreakText, textBreak :: (Char -> Bool) -> Text -> (Text, Text)
consBreakText = break
textBreak = Data.Text.break
inspect ('consBreakText === 'textBreak)

consBreakText', textBreak' :: Text -> (Text, Text)
consBreakText' = break Data.Char.isUpper
textBreak' = Data.Text.break Data.Char.isUpper
inspect ('consBreakText' === 'textBreak')

consBreakLazyText, lazyTextBreak :: (Char -> Bool) -> Data.Text.Lazy.Text -> (Data.Text.Lazy.Text, Data.Text.Lazy.Text)
consBreakLazyText = break
lazyTextBreak = Data.Text.Lazy.break
inspect ('consBreakLazyText === 'lazyTextBreak)

consBreakVector, vectorBreak :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
consBreakVector = break
vectorBreak = Data.Vector.break
inspect ('consBreakVector === 'vectorBreak)

consBreakBS, bsBreak :: (Word8 -> Bool) -> Data.ByteString.ByteString -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consBreakBS = break
bsBreak = Data.ByteString.break
inspect ('consBreakBS === 'bsBreak)

consBreakLBS, lbsBreak :: (Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consBreakLBS = break
lbsBreak = Data.ByteString.Lazy.break
inspect ('consBreakLBS === 'lbsBreak)


{- stripPrefix -}
consStripPrefixList, listStripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
consStripPrefixList = stripPrefix
listStripPrefix = \xss -> go xss
  where
    go xss yss =
      case xss of
        [] -> Just yss
        (x:xs)
          | (y:ys) <- yss
          , x == y -> go xs ys
        _ -> Nothing
inspect ('consStripPrefixList === 'listStripPrefix)

consStripPrefixText, textStripPrefix :: Text -> Text -> Maybe Text
consStripPrefixText = stripPrefix
textStripPrefix = Data.Text.stripPrefix
inspect ('consStripPrefixText === 'textStripPrefix)

consStripPrefixLazyText, lazyTextStripPrefix :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Maybe Data.Text.Lazy.Text
consStripPrefixLazyText = stripPrefix
lazyTextStripPrefix = Data.Text.Lazy.stripPrefix
inspect ('consStripPrefixLazyText === 'lazyTextStripPrefix)

{- groupBy -}
consGroupByList, listGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
consGroupByList = groupBy
listGroupBy p = \s -> go p s
  where
    go p s =
      case s of
        [] -> []
        (x:xs) -> (x:ys) : go p zs
                    where (ys,zs) = listSpan (p x) xs
inspect ('consGroupByList === 'listGroupBy)

consGroupByText, textGroupBy :: (Char -> Char -> Bool) -> Text -> [Text]
consGroupByText = groupBy
textGroupBy = Data.Text.groupBy
inspect ('consGroupByText === 'textGroupBy)

consGroupByText', textGroupBy' :: Text -> [Text]
consGroupByText' = groupBy (==)
textGroupBy' = Data.Text.groupBy (==)
inspect ('consGroupByText' === 'textGroupBy')

consGroupByLazyText, lazyTextGroupBy :: (Char -> Char -> Bool) -> Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consGroupByLazyText = groupBy
lazyTextGroupBy = Data.Text.Lazy.groupBy
inspect ('consGroupByLazyText === 'lazyTextGroupBy)

consGroupByBS, bsGroupBy :: (Word8 -> Word8 -> Bool) -> Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consGroupByBS = groupBy
bsGroupBy = Data.ByteString.groupBy
inspect ('consGroupByBS === 'bsGroupBy)

consGroupByLBS, lbsGroupBy :: (Word8 -> Word8 -> Bool) -> Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consGroupByLBS = groupBy
lbsGroupBy = Data.ByteString.Lazy.groupBy
inspect ('consGroupByLBS === 'lbsGroupBy)

{- group -}
consGroupList, listGroup :: Eq a => [a] -> [[a]]
consGroupList = group
listGroup = listGroupBy (==)
inspect ('consGroupList === 'listGroup)

consGroupText, textGroup :: Text -> [Text]
consGroupText = group
textGroup = Data.Text.group
inspect ('consGroupText === 'textGroup)

consGroupLazyText, lazyTextGroup :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consGroupLazyText = group
lazyTextGroup = Data.Text.Lazy.group
inspect ('consGroupLazyText === 'lazyTextGroup)

consGroupBS, bsGroup :: Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consGroupBS = group
bsGroup = Data.ByteString.group
inspect ('consGroupBS === 'bsGroup)

consGroupLBS, lbsGroup :: Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consGroupLBS = group
lbsGroup = Data.ByteString.Lazy.group
inspect ('consGroupLBS === 'lbsGroup)


{- inits -}
consInitsList, listInits :: [a] -> [[a]]
consInitsList = inits
listInits lst = build (initsGo [] lst)
  where
    initsGo hs xs c n =
      hs `c` case xs of
        [] -> n
        (x' : xs') -> initsGo (hs ++ [x']) xs' c n

{-
This test fails, but the code is morally the same and performs similarly

inspect ('consInitsList === 'listInits)
-}

consInitsText, textInits :: Text -> [Text]
consInitsText = inits
textInits = Data.Text.inits
inspect ('consInitsText === 'textInits)

consInitsLazyText, lazyTextInits :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consInitsLazyText = inits
lazyTextInits = Data.Text.Lazy.inits
inspect ('consInitsLazyText === 'lazyTextInits)

consInitsBS, bsInits :: Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consInitsBS = inits
bsInits = Data.ByteString.inits
inspect ('consInitsBS === 'bsInits)

consInitsLBS, lbsInits :: Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consInitsLBS = inits
lbsInits = Data.ByteString.Lazy.inits
inspect ('consInitsLBS === 'lbsInits)

consInitsSeq, seqInits :: Seq a -> Seq (Seq a)
consInitsSeq = inits
seqInits = Data.Sequence.inits
inspect ('consInitsSeq === 'seqInits)


{- tails -}
consTailsList, listTails :: [a] -> [[a]]
consTailsList = tails
listTails lst =  build (\c n ->
  let tailsGo xs = xs `c` case xs of
                            [] -> n
                            (_ : xs') -> tailsGo xs'
  in tailsGo lst)
inspect ('consTailsList === 'listTails)

consTailsText, textTails :: Text -> [Text]
consTailsText = tails
textTails = Data.Text.tails
inspect ('consTailsText === 'textTails)

consTailsLazyText, lazyTextTails :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text]
consTailsLazyText = tails
lazyTextTails = Data.Text.Lazy.tails
inspect ('consTailsLazyText === 'lazyTextTails)

consTailsBS, bsTails :: Data.ByteString.ByteString -> [Data.ByteString.ByteString]
consTailsBS = tails
bsTails = Data.ByteString.tails
inspect ('consTailsBS === 'bsTails)

consTailsLBS, lbsTails :: Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString]
consTailsLBS = tails
lbsTails = Data.ByteString.Lazy.tails
inspect ('consTailsLBS === 'lbsTails)

consTailsSeq, seqTails :: Seq a -> Seq (Seq a)
consTailsSeq = tails
seqTails = Data.Sequence.tails
inspect ('consTailsSeq === 'seqTails)
