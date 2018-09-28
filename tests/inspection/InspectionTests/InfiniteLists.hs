{- Inspection tests for
== Infinite lists ==
+ iterate
+ iterate'
+ repeat
+ replicate
+ cycle
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.InfiniteLists where

import Control.Applicative (ZipList(..))
import Control.Lens.Prism (nearly)
import Data.Bool ((&&), (||), Bool(..), otherwise)
import Data.Char (Char, isAlpha, isUpper, isLower, toLower, toUpper)
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
import GHC.Base (IO, pure, seq)
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


{- iterate -}
consIterate, listIterate :: (a -> a) -> a -> [a]
consIterate = iterate
listIterate f = go
  where
    go a = a : go (f a)
inspect ('consIterate === 'listIterate)

consIterateLazyText, lazyTextIterate :: (Char -> Char) -> Char -> Data.Text.Lazy.Text
consIterateLazyText = iterate
lazyTextIterate = Data.Text.Lazy.iterate
inspect ('consIterateLazyText === 'lazyTextIterate)

alterLowerUpper :: Char -> Char
alterLowerUpper a =
  case isAlpha a of
    True -> case isUpper a of
              True -> toLower a
              False -> toUpper a
    False -> a

consIterateLazyText1, lazyTextIterate1 :: Char -> Data.Text.Lazy.Text
consIterateLazyText1 = iterate alterLowerUpper
lazyTextIterate1 = Data.Text.Lazy.iterate alterLowerUpper
inspect ('consIterateLazyText1 === 'lazyTextIterate1)

consIterateLazyText2, lazyTextIterate2 :: Data.Text.Lazy.Text
consIterateLazyText2 = iterate alterLowerUpper 'a'
lazyTextIterate2 = Data.Text.Lazy.iterate alterLowerUpper 'a'
inspect ('consIterateLazyText2 === 'lazyTextIterate2)

consIterateLBS, lbsIterate :: (Word8 -> Word8) -> Word8 -> Data.ByteString.Lazy.ByteString
consIterateLBS = iterate
lbsIterate = Data.ByteString.Lazy.iterate
inspect ('consIterateLBS === 'lbsIterate)

consIterateLBS1, lbsIterate1 :: Word8 -> Data.ByteString.Lazy.ByteString
consIterateLBS1 = iterate (\c -> c)
lbsIterate1 = Data.ByteString.Lazy.iterate (\c -> c)
inspect ('consIterateLBS1 === 'lbsIterate1)


{- iterate' -}
consIterate', listIterate' :: (a -> a) -> a -> [a]
consIterate' = iterate'
listIterate' f = go
  where
    go a =
      let
        a' = f a
      in
        a' `seq` (a : go a')
inspect ('consIterate' === 'listIterate')


{- repeat -}
consRepeat, listRepeat :: a -> [a]
consRepeat a = repeat a
listRepeat a = as
  where
    as = a : as
inspect ('consRepeat === 'listRepeat)

consRepeatLazyText, lazyTextRepeat :: Char -> Data.Text.Lazy.Text
consRepeatLazyText = repeat
lazyTextRepeat = Data.Text.Lazy.repeat
inspect ('consRepeatLazyText === 'lazyTextRepeat)

consRepeatLBS, lbsRepeat :: Word8 -> Data.ByteString.Lazy.ByteString
consRepeatLBS = repeat
lbsRepeat = Data.ByteString.Lazy.repeat
inspect ('consRepeatLBS === 'lbsRepeat)


{- replicate -}
consReplicate, listReplicate :: Int -> a -> [a]
consReplicate = replicate
listReplicate n a
  | 0 < n = go n
  | otherwise = []
  where
    go 1 = [a]
    go n = a : go (n-1)
inspect ('consReplicate === 'listReplicate)

consReplicate1, listReplicate1 :: [Char]
consReplicate1 = replicate 100 'a'
listReplicate1 = Data.List.replicate 100 'a'
inspect ('consReplicate1 === 'listReplicate1)

consReplicateMap, listReplicateMap :: [Int]
consReplicateMap = map (+10) (replicate 100 10 :: [Int])
listReplicateMap = Data.List.map (+10) (Data.List.replicate 100 10)
inspect ('consReplicateMap === 'listReplicateMap)

consReplicateMap', listReplicateMap' :: Int -> [Int]
consReplicateMap' n = map (+10) (replicate n 10 :: [Int])
listReplicateMap' n = Data.List.map (+10) (Data.List.replicate n 10)
inspect ('consReplicateMap' === 'listReplicateMap')

{-# noinline consReplicateText #-}
{-# noinline textReplicate #-}
consReplicateText, textReplicate :: Int -> Char -> Text
consReplicateText = replicate
textReplicate n = Data.Text.replicate n . Data.Text.singleton
inspect ('consReplicateText === 'textReplicate)


{- cycle -}
consCycle, listCycle :: [a] -> [a]
consCycle = cycle
listCycle = \as ->
  case as of
    [] -> errorEmptyList "cycle"
    _  -> as' where as' = append as as'
inspect ('consCycle === 'listCycle)

consCycleLazyText, lazyTextCycle :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consCycleLazyText = cycle
lazyTextCycle = Data.Text.Lazy.cycle
inspect ('consCycleLazyText === 'lazyTextCycle)

consCycleLBS, lbsCycle :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
consCycleLBS = cycle
lbsCycle = Data.ByteString.Lazy.cycle
inspect ('consCycleLBS === 'lbsCycle)

-- consCycle', listCycle' :: [Char]
-- consCycle' = cycle [1,2,3]
-- listCycle' = Data.List.cycle [1,2,3]
-- consCycle' = Data.List.take 10 (Consy.cycle "abc")
-- listCycle' = Data.List.take 10 (Data.List.cycle "abc")
-- inspect ('consCycle' === 'listCycle')
