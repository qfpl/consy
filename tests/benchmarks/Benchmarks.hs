{-# language NoImplicitPrelude #-}
module Main where

import Criterion.Main

import Control.Applicative (ZipList(..), pure)
import Data.Function (($), (.))
import Data.Ord ((>))
import Data.Text (Text, pack)
import Data.Int (Int, Int64)
import GHC.Enum (succ)
import GHC.Num (Integer, (+))
import GHC.Base (IO)

import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text.Lazy

import Consy
import InspectionTests

main :: IO ()
main =
  defaultMain
    [ env (pure $ Data.Sequence.fromList [1..1000::Int]) $
      \input -> bgroup "sequence filter"
      [ bench "cons filter seq" $ nf (consFilterSeq (>500)) input
      , bench "seq filter" $ nf (seqFilter (>500)) input
      ]
    , env (pure $ Data.Sequence.fromList [1..1000::Int]) $
      \input -> bgroup "sequence foldl'"
      [ bench "cons foldl' seq" $ nf (consFoldl'Seq (\a _ -> a + 1 :: Int) 0) input
      , bench "seq foldl'" $ nf (seqFoldl' (\a _ -> a + 1 :: Int) 0) input
      ]
    , env (pure . pack $ Data.List.replicate 1000 'a') $
      \input -> bgroup "text length"
      [ bench "cons length text" $ nf consLength input
      , bench "text length" $ nf textLength input
      , bench "cons foldr text" $ nf consFoldrLength input
      , bench "text foldr" $ nf textFoldrLength input
      , bench "text map foldr" $ nf (textMapFoldrLength succ) input
      , bench "cons map foldr text" $ nf (consMapFoldrTextLength succ) input
      , bench "cons foldl' text" $ nf consFoldl'Length input
      , bench "text foldl'" $ nf textFoldl'Length input
      ]
    , env (pure . Data.ByteString.Char8.pack $ Data.List.replicate 1000 'a') $
      \input -> bgroup "bs length"
      [ bench "cons foldr bs" $ nf consFoldrLengthBS input
      , bench "bs foldr" $ nf bsFoldrLength input
      ]
    , env (pure . Data.ByteString.Lazy.Char8.pack $ Data.List.replicate 1000 'a') $
      \input -> bgroup "lbs length"
      [ bench "cons foldr lbs" $ nf consFoldrLengthLBS input
      , bench "lbs foldr" $ nf lbsFoldrLength input
      ]
    , env (pure . Data.Text.Lazy.pack $ Data.List.replicate 1000 'a') $
      \input -> bgroup "ltext length"
      [ bench "cons foldr ltext" $ nf consFoldrLengthLText input
      , bench "ltext foldr" $ nf ltextFoldrLength input
      ]
    , env (pure $ Data.List.replicate 1000 'a') $
      \input -> bgroup "list length"
      [ bench "cons length list" $ nf consListLength input
      , bench "list length" $ nf listLength input
      ]
    , env (pure . Data.Sequence.fromList $ Data.List.replicate 1000 'a') $
      \input -> bgroup "seq length"
      [ bench "cons foldr length seq" $ nf consFoldrLengthSeq input
      , bench "seq foldr length" $ nf seqFoldrLength input
      , bench "cons length seq" $ nf consLengthSeq input
      , bench "seq length" $ nf seqLength input
      ]
    , env (pure . Data.Sequence.fromList $ Data.List.replicate 1000 (10::Int)) $
      \input -> bgroup "seq map"
      [ bench "cons map seq" $ nf (consMapSeq (+10)) input
      , bench "seq map" $ nf (seqMap (+10)) input
      ]
    , env (pure . Data.Sequence.fromList $ Data.List.replicate 1000 (10::Int)) $
      \input -> bgroup "seq take"
      [ bench "cons take seq" $ nf (consTakeSeq 500) input
      , bench "seq take" $ nf (seqTake 500) input
      ]
    , env (pure . Data.Sequence.fromList $ Data.List.replicate 1000 (10::Int)) $
      \input -> bgroup "seq drop"
      [ bench "cons drop seq" $ nf (consDropSeq 500) input
      , bench "seq drop" $ nf (seqDrop 500) input
      ]
    , env (pure . pack $ Data.List.replicate 1000 'a') $
      \input -> bgroup "text filterMap"
      [ bench "cons" $ nf consFilterMapText input
      , bench "text" $ nf textFilterMap input
      ]
    , bgroup "text replicate"
      [ bench "cons replicate text" $ nf (consReplicateText 1000) 'a'
      , bench "text replicate" $ nf (textReplicate 1000) 'a'
      ]
    , env (pure $ Data.List.replicate 1000 'a') $
      \input -> bgroup "list take"
      [ bench "cons take list" $ nf (consTakeList 500) input
      , bench "list take" $ nf (listTake 500) input
      , bench "cons take ziplist" $ nf (consTakeZipList 500) (ZipList input)
      , bench "ziplist take" $ nf (zipListTake 500) (ZipList input)
      ]
    ]
