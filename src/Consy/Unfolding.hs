{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.Unfolding
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , unfoldr
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Folds (build)


{-# inline [2] unfoldr #-}
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr :: (AsEmpty s, Cons s s a a) => (b -> Maybe (a, b)) -> b -> s
unfoldr =
  \f b0 ->
  build
    (\c n ->
       let
         go b = case f b of
           Just (a, new_b) -> a `c` go new_b
           Nothing         -> n
       in go b0)
{-# rules
"cons unfoldr text" [~2]
    unfoldr @Text = \f -> Data.Text.unfoldr f
"cons unfoldr text eta" [~2]
    forall f b.
    unfoldr @Text f b = Data.Text.unfoldr f b

"cons unfoldr ltext" [~2]
    unfoldr @Data.Text.Lazy.Text = \f -> Data.Text.Lazy.unfoldr f
"cons unfoldr ltext eta" [~2]
    forall f b.
    unfoldr @Data.Text.Lazy.Text f b = Data.Text.Lazy.unfoldr f b

"cons unfoldr bs" [~2]
    unfoldr @BS.ByteString = \f -> BS.unfoldr f
"cons unfoldr bs eta" [~2]
    forall f b.
    unfoldr @BS.ByteString f b = BS.unfoldr f b

"cons unfoldr lbs" [~2]
    unfoldr @LBS.ByteString = \f -> LBS.unfoldr f
"cons unfoldr lbs eta" [~2]
    forall f b.
    unfoldr @LBS.ByteString f b = LBS.unfoldr f b

"cons unfoldr seq" [~2]
    unfoldr @(Seq _) = \f -> Data.Sequence.unfoldr f
"cons unfoldr seq eta" [~2]
    forall f b.
    unfoldr @(Seq _) f b = Data.Sequence.unfoldr f b
#-}
