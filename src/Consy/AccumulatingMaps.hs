{-
== Accumulating maps ==
mapAccumL
mapAccumR
-}

{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
module Consy.AccumulatingMaps
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  -- , mapAccumL
  -- , mapAccumR
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Control.Lens.Prism (Prism', prism')
import Control.Monad (guard)
import Data.Bool ((&&), (||), not, Bool(..), otherwise)
import Data.Char (Char)
import Data.Int (Int)
import Data.Eq (Eq(..))
import Data.Function ((.), id, const)
import Data.Functor ((<$))
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Base (oneShot, seq)
import GHC.List (errorEmptyList)
import GHC.Num ((+), (-))
import GHC.Real (Integral, fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector


{- ___ Accumulating maps ____________________________________________________ -}

-- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
-- mapAccumL f s t = runStateL (traverse (StateL . flip f) t) s
--
--
--
-- mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
-- mapAccumR f s t = runStateR (traverse (StateR . flip f) t) s
