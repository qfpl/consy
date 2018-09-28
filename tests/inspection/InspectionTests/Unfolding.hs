{- Inspection tests for
== Unfolding ==
+ unfoldr
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.Unfolding where

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

{- unfoldr -}
consListUnfoldr, listUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
consListUnfoldr = unfoldr
listUnfoldr f b0 =
  build
  (\c n ->
      let
        go b = case f b of
          Just (a, new_b) -> a `c` go new_b
          Nothing         -> n
      in go b0)
inspect ('consListUnfoldr === 'listUnfoldr)

consUnfoldrText, textUnfoldr :: (a -> Maybe (Char, a)) -> a -> Text
consUnfoldrText = unfoldr
textUnfoldr = Data.Text.unfoldr
inspect ('consUnfoldrText === 'textUnfoldr)

consUnfoldrLazyText, lazyTextUnfoldr :: (a -> Maybe (Char, a)) -> a -> Data.Text.Lazy.Text
consUnfoldrLazyText = unfoldr
lazyTextUnfoldr = Data.Text.Lazy.unfoldr
inspect ('consUnfoldrLazyText === 'lazyTextUnfoldr)

consUnfoldrBS, bsUnfoldr :: (a -> Maybe (Word8, a)) -> a -> Data.ByteString.ByteString
consUnfoldrBS = unfoldr
bsUnfoldr = Data.ByteString.unfoldr
inspect ('consUnfoldrBS === 'bsUnfoldr)

consUnfoldrLBS, lbsUnfoldr :: (a -> Maybe (Word8, a)) -> a -> Data.ByteString.Lazy.ByteString
consUnfoldrLBS = unfoldr
lbsUnfoldr = Data.ByteString.Lazy.unfoldr
inspect ('consUnfoldrLBS === 'lbsUnfoldr)

consUnfoldrSeq, seqUnfoldr ::  (b -> Maybe (a, b)) -> b -> Data.Sequence.Seq a
consUnfoldrSeq = unfoldr
seqUnfoldr = Data.Sequence.unfoldr
inspect ('consUnfoldrSeq === 'seqUnfoldr)
