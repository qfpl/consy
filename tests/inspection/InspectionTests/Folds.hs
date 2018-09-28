{- Inspection tests for
== Reducing lists (folds) ==
+ foldl
+ foldl'
+ foldl1
+ foldl1'
+ foldr
+ foldr1
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.Folds where

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
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Vector (Vector)
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


{- foldl -}
consFoldlText, textFoldl :: (a -> Char -> a) -> a -> Text -> a
consFoldlText = foldl
textFoldl = Data.Text.foldl
inspect ('consFoldlText === 'textFoldl)

consFoldlText2, textFoldl2 :: Int -> Text -> Int
consFoldlText2 = foldl (\a _ -> a + 1)
textFoldl2 = Data.Text.foldl (\a _ -> a + 1)
inspect ('consFoldlText2 === 'textFoldl2)

consFoldlText4, textFoldl4 :: Int
consFoldlText4 = foldl (\a _ -> a + 1) 0 (pack "aaaa")
textFoldl4 = Data.Text.foldl (\a _ -> a + 1) 0 (pack "aaaa")
inspect ('consFoldlText4 === 'textFoldl4)

{-# noinline consFoldlSeq #-}
{-# noinline seqFoldl #-}
consFoldlSeq, seqFoldl :: (b -> a -> b) -> b -> Seq a -> b
consFoldlSeq = foldl
seqFoldl = Data.Foldable.foldl
inspect ('consFoldlSeq === 'seqFoldl)


{- foldl' -}
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

{-# noinline consFoldl'Seq #-}
{-# noinline seqFoldl' #-}
consFoldl'Seq, seqFoldl' :: (b -> a -> b) -> b -> Seq a -> b
consFoldl'Seq = foldl'
seqFoldl' = Data.Foldable.foldl'
inspect ('consFoldl'Seq === 'seqFoldl')


{- foldl1 -}
consFoldl1Text, textFoldl1 :: (Char -> Char -> Char) -> Text -> Char
consFoldl1Text = foldl1
textFoldl1 = Data.Text.foldl1
inspect ('consFoldl1Text === 'textFoldl1)

consFoldl1Text4, textFoldl14 :: Char
consFoldl1Text4 = foldl1 (\a _ -> '*')  (pack "aaaa")
textFoldl14 = Data.Text.foldl1 (\a _ -> '*')  (pack "aaaa")
inspect ('consFoldl1Text4 === 'textFoldl14)

{-# noinline consFoldl1Seq #-}
{-# noinline seqFoldl1 #-}
consFoldl1Seq, seqFoldl1 :: (a -> a -> a) -> Seq a -> a
consFoldl1Seq = foldl1
seqFoldl1 = Data.Foldable.foldl1
inspect ('consFoldl1Seq === 'seqFoldl1)


{- foldl1' -}
consFoldl1'Text, textFoldl1' :: (Char -> Char -> Char) -> Text -> Char
consFoldl1'Text = foldl1'
textFoldl1' = Data.Text.foldl1'
inspect ('consFoldl1'Text === 'textFoldl1')

consFoldl1'Text4, textFoldl1'4 :: Char
consFoldl1'Text4 = foldl1' (\a _ -> '*')  (pack "aaaa")
textFoldl1'4 = Data.Text.foldl1' (\a _ -> '*')  (pack "aaaa")
inspect ('consFoldl1'Text4 === 'textFoldl1'4)



{- foldr1 -}
consFoldr1Text, textFoldr1 :: (Char -> Char -> Char) -> Text -> Char
consFoldr1Text = foldr1
textFoldr1 = Data.Text.foldr1
inspect ('consFoldr1Text === 'textFoldr1)

consFoldr1Text4, textFoldr14 :: Char
consFoldr1Text4 = foldr1 (\a _ -> '*')  (pack "aaaa")
textFoldr14 = Data.Text.foldr1 (\a _ -> '*')  (pack "aaaa")
inspect ('consFoldr1Text4 === 'textFoldr14)

{-# noinline consFoldr1Seq #-}
{-# noinline seqFoldr1 #-}
consFoldr1Seq, seqFoldr1 :: (a -> a -> a) -> Seq a -> a
consFoldr1Seq = foldr1
seqFoldr1 = Data.Foldable.foldr1
inspect ('consFoldr1Seq === 'seqFoldr1)
