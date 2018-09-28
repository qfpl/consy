{- Inspection tests for
== Searching lists (Searching by equality) ==
+ elem
+ notElem
+ lookup
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.SearchingByEquality where

import Data.Bool (Bool(..), (&&), (||), otherwise)
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(..))
import Data.Vector (Vector)
import Data.Word (Word8)
import Test.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Vector
import Consy


{- elem -}
consElem, listElem :: Eq a => a -> [a] -> Bool
consElem = elem
-- listElem x = Data.List.any (== x)
listElem x = go x
  where
    go x s =
      case s of
        [] -> False
        (a:as) -> x == a || go x as
inspect ('consElem === 'listElem)

consElemVector, vectorElem :: Eq a => a -> Vector a -> Bool
consElemVector = elem
vectorElem = Data.Vector.elem
inspect ('consElemVector === 'vectorElem)

consElemBS, bsElem :: Word8 -> Data.ByteString.ByteString -> Bool
consElemBS = elem
bsElem = Data.ByteString.elem
inspect ('consElemBS === 'bsElem)

consElemLBS, lbsElem :: Word8 -> Data.ByteString.Lazy.ByteString -> Bool
consElemLBS = elem
lbsElem = Data.ByteString.Lazy.elem
inspect ('consElemLBS === 'lbsElem)


{- notElem -}
consNotElem, listNotElem :: Eq a => a -> [a] -> Bool
consNotElem = notElem
listNotElem x = go x
  where
    go x s =
      case s of
        [] -> True
        (a:as) -> x /= a && go x as
inspect ('consNotElem === 'listNotElem)

consNotElemVector, vectorNotElem :: Eq a => a -> Vector a -> Bool
consNotElemVector = notElem
vectorNotElem = Data.Vector.notElem
inspect ('consNotElemVector === 'vectorNotElem)

consNotElemBS, bsNotElem :: Word8 -> Data.ByteString.ByteString -> Bool
consNotElemBS = notElem
bsNotElem = Data.ByteString.notElem
inspect ('consNotElemBS === 'bsNotElem)

consNotElemLBS, lbsNotElem :: Word8 -> Data.ByteString.Lazy.ByteString -> Bool
consNotElemLBS = notElem
lbsNotElem = Data.ByteString.Lazy.notElem
inspect ('consNotElemLBS === 'lbsNotElem)


{- lookup -}
consLookup, listLookup :: (Eq a) => a -> [(a,b)] -> Maybe b
consLookup = lookup
listLookup = go
  where
    go _ [] = Nothing
    go  key ((x,y):xys)
        | x == key  =  Just y
        | otherwise =  go key xys
inspect ('consLookup === 'listLookup)
