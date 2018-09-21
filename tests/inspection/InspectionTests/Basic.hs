{- Inspection tests for
== Basic functions ==
+ append (++)
+ head
+ last
+ tail
+ init
? uncons
+ null
+ length
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin -ddump-to-file -ddump-simpl -ddump-simpl-stats #-}
module InspectionTests.Basic where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.))
import Data.Int (Int, Int64)
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>))
import Data.Sequence (Seq, (><))
import Data.Text (Text, pack)
import Data.Vector (Vector, (++))
import Data.Word (Word8)
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.List (errorEmptyList)
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


{- append -}
consListAppend, listAppend :: [a] -> [a] -> [a]
consListAppend = append
listAppend [] ys = ys
listAppend (x:xs) ys = x : listAppend xs ys
inspect ('consListAppend === 'listAppend)

consAppendText, textAppend :: Text -> Text -> Text
consAppendText = append
textAppend = Data.Text.append
inspect ('consAppendText === 'textAppend)

consAppendLazyText, lazyTextAppend :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consAppendLazyText = append
lazyTextAppend = Data.Text.Lazy.append
inspect ('consAppendLazyText === 'lazyTextAppend)

consAppendVector, vectorAppend :: Vector a -> Vector a -> Vector a
consAppendVector = append
vectorAppend = (Data.Vector.++)
inspect ('consAppendVector === 'vectorAppend)

consAppendBS, bsAppend :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consAppendBS = append
bsAppend = Data.ByteString.append
inspect ('consAppendBS === 'bsAppend)

consAppendLBS, lbsAppend :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consAppendLBS = append
lbsAppend = Data.ByteString.Lazy.append
inspect ('consAppendLBS === 'lbsAppend)

consAppendSeq, seqAppend :: Data.Sequence.Seq a -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consAppendSeq = append
seqAppend = (><)
inspect ('consAppendSeq === 'seqAppend)


{- head -}
-- !!FAILS
-- consListHead, listHead :: [a] -> a
-- consListHead = head
-- listHead [] = badHead
-- listHead(x:_) = x
-- badHead :: a
-- badHead = errorEmptyList "head"
-- inspect ('consListHead === 'listHead)

consHeadText, textHead :: Text -> Char
consHeadText = head
textHead = Data.Text.head
inspect ('consHeadText === 'textHead)

consHeadLazyText, lazyTextHead :: Data.Text.Lazy.Text -> Char
consHeadLazyText = head
lazyTextHead = Data.Text.Lazy.head
inspect ('consHeadLazyText === 'lazyTextHead)

consHeadVector, vectorHead :: Vector a -> a
consHeadVector = head
vectorHead = Data.Vector.head
inspect ('consHeadVector === 'vectorHead)

consHeadBS, bsHead :: Data.ByteString.ByteString -> Data.Word.Word8
consHeadBS = head
bsHead = Data.ByteString.head
inspect ('consHeadBS === 'bsHead)

consHeadLBS, lbsHead :: Data.ByteString.Lazy.ByteString -> Data.Word.Word8
consHeadLBS = head
lbsHead = Data.ByteString.Lazy.head
inspect ('consHeadLBS === 'lbsHead)


{- last -}
-- !!FAILS
-- consListLast, listLast :: [a] -> a
-- consListLast = last
-- listLast = \xs -> foldr (\_ x -> x) (errorEmptyList "tail")  xs
-- inspect ('consListLast === 'listLast)

consLastText, consFoldrLast, textFoldrLast, textLast :: Text -> Char
consLastText = last
textLast = Data.Text.last
consFoldrLast = foldr (\_ x-> x) (errorEmptyList "tail")
textFoldrLast = Data.Text.foldr (\_ x -> x) (errorEmptyList "tail")
inspect ('consLastText === 'textLast)
inspect ('consFoldrLast === 'textFoldrLast)

consLastLazyText, lazyTextLast :: Data.Text.Lazy.Text -> Char
consLastLazyText = last
lazyTextLast = Data.Text.Lazy.last
inspect ('consLastLazyText === 'lazyTextLast)

consLastVector, vectorLast :: Vector a -> a
consLastVector = last
vectorLast = Data.Vector.last
inspect ('consLastVector === 'vectorLast)

consLastBS, bsLast :: Data.ByteString.ByteString -> Data.Word.Word8
consLastBS = last
bsLast = Data.ByteString.last
inspect ('consLastBS === 'bsLast)

consLastLBS, lbsLast :: Data.ByteString.Lazy.ByteString -> Data.Word.Word8
consLastLBS = last
lbsLast = Data.ByteString.Lazy.last
inspect ('consLastLBS === 'lbsLast)


{- tail -}
-- !!FAILS
-- consListTail, listTail :: [a] -> [a]
-- consListTail = tail
-- listTail (_:xs) = xs
-- listTail [] = errorEmptyList "tail"
-- inspect ('consListTail === 'listTail)

consTailText, textTail :: Text -> Text
consTailText = tail
textTail = Data.Text.tail
inspect ('consTailText === 'textTail)

consTailLazyText, lazyTextTail :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consTailLazyText = tail
lazyTextTail = Data.Text.Lazy.tail
inspect ('consTailLazyText === 'lazyTextTail)

consTailVector, vectorTail :: Vector a -> Vector a
consTailVector = tail
vectorTail = Data.Vector.tail
inspect ('consTailVector === 'vectorTail)

consTailBS, bsTail :: Data.ByteString.ByteString -> Data.ByteString.ByteString
consTailBS = tail
bsTail = Data.ByteString.tail
inspect ('consTailBS === 'bsTail)

consTailLBS, lbsTail :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consTailLBS = tail
lbsTail = Data.ByteString.Lazy.tail
inspect ('consTailLBS === 'lbsTail)


{- init -}
consListInit, listInit :: [a] -> [a]
consListInit = init
listInit = go
  where
    go s =
      case s of
        [] -> errorEmptyList "init"
        x : xs -> init' x xs
          where init' _ []     = []
                init' y (z:zs) = y : init' z zs
inspect ('consListInit === 'listInit)

consInitText, textInit :: Text -> Text
consInitText = init
textInit = Data.Text.init
inspect ('consInitText === 'textInit)

consInitLazyText, lazyTextInit :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consInitLazyText = init
lazyTextInit = Data.Text.Lazy.init
inspect ('consInitLazyText === 'lazyTextInit)

consInitVector, vectorInit :: Vector a -> Vector a
consInitVector = init
vectorInit = Data.Vector.init
inspect ('consInitVector === 'vectorInit)

consInitBS, bsInit :: Data.ByteString.ByteString -> Data.ByteString.ByteString
consInitBS = init
bsInit = Data.ByteString.init
inspect ('consInitBS === 'bsInit)

consInitLBS, lbsInit :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consInitLBS = init
lbsInit = Data.ByteString.Lazy.init
inspect ('consInitLBS === 'lbsInit)


{- null -}
consListNull, listNull :: [a] -> Bool
consListNull = null
listNull = go
  where
    go s =
      case s of
        [] -> True
        x : _ -> False
inspect ('consListNull === 'listNull)

consNullText, textNull :: Text -> Bool
consNullText = null
textNull = Data.Text.null
inspect ('consNullText === 'textNull)

consNullLazyText, lazyTextNull :: Data.Text.Lazy.Text -> Bool
consNullLazyText = null
lazyTextNull = Data.Text.Lazy.null
inspect ('consNullLazyText === 'lazyTextNull)

consNullVector, vectorNull :: Vector a -> Bool
consNullVector = null
vectorNull = Data.Vector.null
inspect ('consNullVector === 'vectorNull)

consNullBS, bsNull :: Data.ByteString.ByteString -> Bool
consNullBS = null
bsNull = Data.ByteString.null
inspect ('consNullBS === 'bsNull)

consNullLBS, lbsNull :: Data.ByteString.Lazy.ByteString -> Bool
consNullLBS = null
lbsNull = Data.ByteString.Lazy.null
inspect ('consNullLBS === 'lbsNull)

consNullSeq, seqNull :: Data.Sequence.Seq a -> Bool
consNullSeq = null
seqNull = Data.Sequence.null
inspect ('consNullSeq === 'seqNull)


{- length -}
consListLength, listLength :: [a] -> Int
consListLength = length
listLength = go 0
  where
    go !n s =
      case s of
        [] -> n
        _ : xs -> go (n + 1) xs
inspect ('consListLength === 'listLength)

-- QUESTION why these noinline switches?
-- with these switches rules `cons foldr text` and `cons length text`
--  are still fires
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

consFoldrLengthLText, ltextFoldrLength :: Data.Text.Lazy.Text -> Int
consFoldrLengthLText = foldr (\_ -> (+1)) 0
ltextFoldrLength = Data.Text.Lazy.foldr (\_ -> (+1)) 0
inspect ('consFoldrLengthLText === 'ltextFoldrLength)

consFoldrLengthBS, bsFoldrLength :: Data.ByteString.ByteString -> Int
consFoldrLengthBS = foldr (\_ -> (+1)) 0
bsFoldrLength = Data.ByteString.foldr (\_ -> (+1)) 0
inspect ('consFoldrLengthBS === 'bsFoldrLength)

consFoldrLengthLBS, lbsFoldrLength :: Data.ByteString.Lazy.ByteString -> Int
consFoldrLengthLBS = foldr (\_ -> (+1)) 0
lbsFoldrLength = Data.ByteString.Lazy.foldr (\_ -> (+1)) 0
inspect ('consFoldrLengthLBS === 'lbsFoldrLength)

consFoldrLengthSeq, seqFoldrLength :: Data.Sequence.Seq a -> Int
consFoldrLengthSeq = foldr (\_ -> (+1)) 0
seqFoldrLength = Data.Foldable.foldr (\_ -> (+1)) 0
inspect ('consFoldrLengthSeq === 'seqFoldrLength)

consLengthSeq, seqLength :: Data.Sequence.Seq a -> Int
consLengthSeq = length
seqLength = Data.Sequence.length
inspect ('consLengthSeq === 'seqLength)
