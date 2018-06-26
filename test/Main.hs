{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin -ddump-simpl #-}
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
import Weigh

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


consFilterText, textFilter :: Text
consFilterText = filter (== 'a') (pack "bbbb")
textFilter = Data.Text.filter (== 'a') (pack "bbbb")

inspect ('consFilterText === 'textFilter)

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
consFoldrLength = foldr (\_ -> (+1)) 0
textFoldrLength = Data.Text.foldr (\_ -> (+1)) 0
textLength = Data.Text.length

-- These are not equal
-- inspect ('consLengthText === 'textFoldrLength)

{-# noinline consFoldl'Length #-}
{-# noinline textFoldl'Length #-}
consFoldl'Length, textFoldl'Length :: Text -> Int
consFoldl'Length = foldl' (\b _ -> b + 1) 0
textFoldl'Length = Data.Text.foldl' (\b _ -> b + 1) 0

-- These are not equal either
-- inspect ('consFoldl' === 'listFoldl')

main :: IO ()
main = do
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
    ]

  mainWith $ do
    func "cons foldr length" consFoldrLength (pack $ Data.List.replicate 1000 'a')
    func "foldr length" textFoldrLength (pack $ Data.List.replicate 1000 'a')
    func "cons length text" consLength (pack $ Data.List.replicate 1000 'a')
    func "text length" textLength (pack $ Data.List.replicate 1000 'a')
