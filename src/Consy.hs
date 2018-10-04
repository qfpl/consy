{-# language NoImplicitPrelude #-}
module Consy
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
    -- * Basic functions
  , append
  , head
  , last
  , tail
  , init
  , null
  , length
    -- * Transformations
  , map
  , traverse
  , reverse
  , intersperse
  , intercalate
  , transpose
  , subsequences
  , permutations
    -- * Reducing
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr1
  , foldMap
  , augment
  , build
    -- ** Special folds
  , concat
  , concatMap
  , and
  , or
  , any
  , all
  , sum
  , product
  , maximum
  , minimum
    -- * Building
    -- ** Scans
  , scanl
  , scanl'
  , scanl1
  , scanr
  , scanr1
    -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
    -- ** Infinite structures
  , iterate
  , iterate'
  , repeat
  , replicate
  , cycle
    -- ** Unfolding
  , unfoldr
    -- * Sublists
    -- ** Extracting substructures
  , take
  , drop
  , splitAt
  , takeWhile
  , dropWhile
  , dropWhileEnd
  , span
  , break
  , stripPrefix
  , group
  , groupBy
  , inits
  , tails
    -- ** Predicates
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , isSubsequenceOf
    -- * Searching
    -- ** Searching by equality
  , elem
  , notElem
  , lookup
    -- ** Searching with a predicate
  , find
  , filter
  , partition
    -- * Indexing
  , (!!)
  , elemIndex
  , elemIndices
  , findIndex
  , findIndices
    -- * Zipping and unzipping
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , zip7
  , zipWith
  , zipWith3
  , zipWith4
  , zipWith5
  , zipWith6
  , zipWith7
  , unzip
  , unzip3
  , unzip4
  , unzip5
  , unzip6
  , unzip7
  )
where

import Control.Lens.Cons
import Control.Lens.Empty

import Consy.Basic
import Consy.TransformationsMap
import Consy.Transformations
import Consy.Folds
import Consy.SpecialFolds
import Consy.AccumulatingMaps
import Consy.InfiniteLists
import Consy.Unfolding
import Consy.ExtractingSublists
import Consy.Scans
import Consy.SublistsWithPredicates
import Consy.SearchingByEquality
import Consy.SearchingWithPredicate
import Consy.Traversable
import Consy.Indexing
import Consy.Zipping
