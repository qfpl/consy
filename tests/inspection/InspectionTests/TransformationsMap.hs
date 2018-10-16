{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.TransformationsMap where

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
import GHC.Base (IO, pure, flip)
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


{- map -}
consMapList, listMap :: (a -> b) -> [a] -> [b]
consMapList = map
listMap f = go
  where
    go [] = []
    go (x:xs) = f x : go xs
inspect ('consMapList === 'listMap)

consMapList2, consMapList2' :: forall a b c. (b -> c) -> (a -> b) -> [a] -> [c]
consMapList2 g f = map g . (map f :: [a] -> [b])
consMapList2' g f = map (g . f)
inspect ('consMapList2 === 'consMapList2')

consFoldrListLength, listFoldrLength :: [a] -> Int
consFoldrListLength = foldr (\_ -> (+1)) 0
listFoldrLength = Data.List.foldr (\_ -> (+1)) 0
inspect ('consFoldrListLength === 'listFoldrLength)

consMapFoldrTextLength, textMapFoldrLength :: (Char -> Char) -> Text -> Int
consMapFoldrTextLength f = foldr (\_ -> (+1)) 0 . (map f :: Text -> Text)
textMapFoldrLength f = Data.Text.foldr (\_ -> (+1)) 0 . Data.Text.map f
inspect ('consMapFoldrTextLength === 'textMapFoldrLength)

consMapSeq, seqMap :: (a -> a) -> Data.Sequence.Seq a -> Data.Sequence.Seq a
consMapSeq = map
seqMap = Data.Functor.fmap
inspect ('consMapSeq === 'seqMap)
