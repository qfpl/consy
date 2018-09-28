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
  , reverse
  , intersperse
  , intercalate
  , transpose
  , subsequences
  -- , permutations
    -- * Reducing
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr1
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
  -- , mapAccumL
  -- , mapAccumR
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

import Consy.Basic                  -- done
import Consy.TransformationsMap     -- done, contains only `map` due to recursive import
import Consy.Transformations
import Consy.Folds
import Consy.SpecialFolds           -- done
-- import Consy.AccumulatingMaps      -- missing all
import Consy.InfiniteLists          -- done
import Consy.Unfolding              -- done
import Consy.ExtractingSublists     -- done
import Consy.Scans
import Consy.SublistsWithPredicates -- done
import Consy.SearchingByEquality    -- done
import Consy.SearchingWithPredicate -- done
import Consy.Indexing               -- done
import Consy.Zipping                -- done
