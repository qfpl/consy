{-# language NoImplicitPrelude #-}
module Consy.Eq where

import Control.Lens.Cons (Cons, uncons)
import Data.Bool (Bool(..), (&&))
import Data.Eq (Eq)
import Data.Maybe (Maybe(..))

import qualified Data.Eq

(==) :: (Eq a, Cons s s a a) => s -> s -> Bool
(==) = go
  where
    go a b =
      case (uncons a, uncons b) of
        (Nothing, Nothing) -> True
        (Just (x, xs), Just (y, ys)) ->
          x Data.Eq.== y && go xs ys
        (_, _) -> False
infix 4 ==
