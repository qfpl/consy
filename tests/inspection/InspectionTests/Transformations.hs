{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language TemplateHaskell #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.Transformations where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq(..), (==))
import Data.Function (($), (.), id)
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


{- reverse -}
consListReverse, listReverse :: [a] -> [a]
consListReverse = reverse
listReverse = foldl (flip (:)) []
inspect ('consListReverse === 'listReverse)

consReverseText, textReverse :: Text -> Text
consReverseText = reverse
textReverse = Data.Text.reverse
inspect ('consReverseText === 'textReverse)

consReverseLazyText, lazyTextReverse :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consReverseLazyText = reverse
lazyTextReverse = Data.Text.Lazy.reverse
inspect ('consReverseLazyText === 'lazyTextReverse)

consReverseVector, vectorReverse :: Vector a -> Vector a
consReverseVector = reverse
vectorReverse = Data.Vector.reverse
inspect ('consReverseVector === 'vectorReverse)

consReverseBS, bsReverse :: Data.ByteString.ByteString -> Data.ByteString.ByteString
consReverseBS = reverse
bsReverse = Data.ByteString.reverse
inspect ('consReverseBS === 'bsReverse)

consReverseLBS, lbsReverse :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consReverseLBS = reverse
lbsReverse = Data.ByteString.Lazy.reverse
inspect ('consReverseLBS === 'lbsReverse)

consReverseSeq, seqReverse :: Data.Sequence.Seq a -> Data.Sequence.Seq a
consReverseSeq = reverse
seqReverse = Data.Sequence.reverse
inspect ('consReverseSeq === 'seqReverse)


{- intersperse -}
consListIntersperse, listIntersperse :: a -> [a] -> [a]
consListIntersperse = intersperse
listIntersperse _ [] = []
listIntersperse sep (x:xs) = x : prependToAll sep xs
  where
    prependToAll :: a -> [a] -> [a]
    prependToAll _ [] = []
    prependToAll sep (x:xs) = sep : x : prependToAll sep xs
inspect ('consListIntersperse === 'listIntersperse)

consIntersperseText, textIntersperse :: Char -> Text -> Text
consIntersperseText = intersperse
textIntersperse = Data.Text.intersperse
inspect ('consIntersperseText === 'textIntersperse)

consIntersperseLazyText, lazyTextIntersperse :: Char -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consIntersperseLazyText = intersperse
lazyTextIntersperse = Data.Text.Lazy.intersperse
inspect ('consIntersperseLazyText === 'lazyTextIntersperse)

consIntersperseBS, bsIntersperse :: Word8 -> Data.ByteString.ByteString -> Data.ByteString.ByteString
consIntersperseBS = intersperse
bsIntersperse = Data.ByteString.intersperse
inspect ('consIntersperseBS === 'bsIntersperse)

consIntersperseLBS, lbsIntersperse :: Word8 -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consIntersperseLBS = intersperse
lbsIntersperse = Data.ByteString.Lazy.intersperse
inspect ('consIntersperseLBS === 'lbsIntersperse)


{- intercalate -}
consListIntercalate, listIntercalate :: [a] -> [[a]] -> [a]
consListIntercalate = intercalate
listIntercalate xs xss = concat (intersperse xs xss)
inspect ('consListIntercalate === 'listIntercalate)

consIntercalateText, textIntercalate :: Text -> [Text] -> Text
consIntercalateText = intercalate
textIntercalate = Data.Text.intercalate
inspect ('consIntercalateText === 'textIntercalate)

consIntercalateLazyText, lazyTextIntercalate :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text] -> Data.Text.Lazy.Text
consIntercalateLazyText = intercalate
lazyTextIntercalate = Data.Text.Lazy.intercalate
inspect ('consIntercalateLazyText === 'lazyTextIntercalate)

consIntercalateBS, bsIntercalate :: Data.ByteString.ByteString -> [Data.ByteString.ByteString] -> Data.ByteString.ByteString
consIntercalateBS = intercalate
bsIntercalate = Data.ByteString.intercalate
inspect ('consIntercalateBS === 'bsIntercalate)

consIntercalateLBS, lbsIntercalate :: Data.ByteString.Lazy.ByteString -> [Data.ByteString.Lazy.ByteString] -> Data.ByteString.Lazy.ByteString
consIntercalateLBS = intercalate
lbsIntercalate = Data.ByteString.Lazy.intercalate
inspect ('consIntercalateLBS === 'lbsIntercalate)


{- transpose -}
consListTranspose, listTranspose :: [[a]] -> [[a]]
consListTranspose = transpose
listTranspose [] = []
listTranspose ([] : xss) = listTranspose xss
listTranspose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : listTranspose (xs : [ t | (_:t) <- xss])
inspect ('consListTranspose === 'listTranspose)

consTransposeText, textTranspose :: [Text] -> [Text]
consTransposeText = transpose
textTranspose = Data.Text.transpose
inspect ('consTransposeText === 'textTranspose)

consTransposeLazyText, lazyTextTranspose :: [Data.Text.Lazy.Text] -> [Data.Text.Lazy.Text]
consTransposeLazyText = transpose
lazyTextTranspose = Data.Text.Lazy.transpose
inspect ('consTransposeLazyText === 'lazyTextTranspose)

consTransposeBS, bsTranspose :: [Data.ByteString.ByteString] -> [Data.ByteString.ByteString]
consTransposeBS = transpose
bsTranspose = Data.ByteString.transpose
inspect ('consTransposeBS === 'bsTranspose)

consTransposeLBS, lbsTranspose :: [Data.ByteString.Lazy.ByteString] -> [Data.ByteString.Lazy.ByteString]
consTransposeLBS = transpose
lbsTranspose = Data.ByteString.Lazy.transpose
inspect ('consTransposeLBS === 'lbsTranspose)

consListSubsequences, listSubsequences :: [a] -> [[a]]
consListSubsequences = subsequences
listSubsequences xs =  [] : nonEmptySubsequences xs
  where
    nonEmptySubsequences :: [a] -> [[a]]
    nonEmptySubsequences [] = []
    nonEmptySubsequences (x:xs) = [x] : foldr f [] (nonEmptySubsequences xs)
      where f ys r = ys : (x : ys) : r
inspect ('consListSubsequences === 'listSubsequences)

consListPermutations, listPermutations :: [a] -> [[a]]
consListPermutations = permutations
listPermutations xs0 = xs0 : perms xs0 []
  where
    perms [] _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (listPermutations is)
      where
        interleave xs r = let (_,zs) = interleave' id xs r in zs
        interleave' _ [] r = (ts, r)
        interleave' f (y:ys) r =
          let
            (us,zs) = interleave' (f . (y:)) ys r
          in  (y:us, f (t:y:us) : zs)
inspect ('consListPermutations === 'listPermutations)
