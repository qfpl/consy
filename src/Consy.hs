{-# language NoImplicitPrelude #-}
-- {-# language BangPatterns #-}
-- {-# language PatternSynonyms #-}
-- {-# language RankNTypes #-}
-- {-# language ScopedTypeVariables #-}
-- {-# language TypeApplications #-}
module Consy
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , append
  , head
  , last
  , tail
  , init
  -- , uncons
  , null
  , length
  -- ,------
  , map
  -- ,------
  , reverse
  , intersperse
  , intercalate
  , transpose
  , subsequences
  -- , permutations
  -- ,------
  , augment
  , build
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr1
  -- ,------
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
  -- , -----
  -- , mapAccumL
  -- , mapAccumR
  -- , -----
  , iterate
  , iterate'
  , repeat
  , replicate
  , cycle
  -- , -----
  , unfoldr
  -- , -----
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
  -- , -----
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , isSubsequenceOf
  -- , -----
  , elem
  , notElem
  , lookup
  -- , -----
  , find
  , filter
  , partition
  -- , -----
  , (!!)
  , elemIndex
  , elemIndices
  , findIndex
  , findIndices
  -- , -----
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
import Consy.Transformations          -- missing permutations
import Consy.Folds
import Consy.SpecialFolds           -- done
-- import Consy.AccumulatingMaps      -- missing all
import Consy.InfiniteLists          -- done
import Consy.Unfolding              -- done
import Consy.ExtractingSublists     -- done
import Consy.SublistsWithPredicates -- done
import Consy.SearchingByEquality    -- done
import Consy.SearchingWithPredicate -- done
import Consy.Indexing               -- done
import Consy.Zipping                -- done
