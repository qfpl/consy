{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module Main where

import Criterion.Main
import Data.Bool (Bool, otherwise)
import Data.Char (Char)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Text (Text, pack)
import qualified Data.Text
import GHC.Base (IO, pure)
import GHC.Enum (succ)
import GHC.Num (Num, (+))
import Test.Inspection

import qualified Data.List
import Consy

consFilter, listFilter :: (a -> Bool) -> [a] -> [a]
consFilter = filter
listFilter p = go
  where
    go [] = []
    go (x : xs)
      | p x = x : go xs
      | otherwise = go xs

inspect ('consFilter === 'listFilter)


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


consFoldl'Text, textFoldl' :: (a -> Char -> a) -> a -> Text -> a
consFoldl'Text = foldl'
textFoldl' = Data.Text.foldl'

inspect ('consFoldl'Text === 'textFoldl')


consFoldl'Text2, textFoldl'2 :: Int -> Text -> Int
consFoldl'Text2 = foldl' (\a _ -> a + 1)
textFoldl'2 = Data.Text.foldl' (\a _ -> a + 1)

inspect ('consFoldl'Text2 === 'textFoldl'2)


consFoldl'Text3, textFoldl'3 :: Text -> Int
consFoldl'Text3 = foldl' (\a _ -> a + 1) 0
textFoldl'3 = Data.Text.foldl' (\a _ -> a + 1) 0

inspect ('consFoldl'Text3 === 'textFoldl'3)


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


consReplicate, listReplicate :: [Char]
consReplicate = replicate 100 'a'
listReplicate = Data.List.replicate 100 'a'

inspect ('consReplicate === 'listReplicate)


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
consReplicateText n x = replicate n x
textReplicate n x = Data.Text.replicate n (Data.Text.singleton x)

inspect ('consReplicateText === 'textReplicate)


consListLength, listLength :: [a] -> Int
consListLength = length
listLength = go 0
  where
    go !n s =
      case s of
        [] -> n
        _ : xs -> go (n + 1) xs

-- These are not equal, although they benchmark identically
-- inspect ('consListLength === 'listLength)


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


main :: IO ()
main =
  defaultMain
    [ env (pure . pack $ Data.List.replicate 1000 'a') $
      \input -> bgroup "text length"
      [ bench "cons length text" $ whnf consLength input
      , bench "text length" $ whnf textLength input
      , bench "cons foldr text" $ whnf consFoldrLength input
      , bench "text foldr" $ whnf textFoldrLength input
      , bench "cons foldl' text" $ whnf consFoldl'Length input
      , bench "text foldl'" $ whnf textFoldl'Length input
      ]
    , env (pure $ Data.List.replicate 1000 'a') $
      \input -> bgroup "list length"
      [ bench "cons length list" $ whnf consListLength input
      , bench "list length" $ whnf listLength input
      ]
    , env (pure . pack $ Data.List.replicate 1000 'a') $
      \input -> bgroup "text filterMap"
      [ bench "cons" $ whnf consFilterMapText input
      , bench "text" $ whnf textFilterMap input
      ]
    , bgroup "text replicate"
      [ bench "cons replicate text" $ whnf (consReplicateText 1000) 'a'
      , bench "text replicate" $ whnf (textReplicate 1000) 'a'
      ]
    ]
