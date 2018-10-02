{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language PatternSynonyms #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
module Consy.Traversable where

import Control.Lens.Empty (AsEmpty, pattern Empty)
import Control.Lens.Cons (Cons, uncons, cons)
import Data.Monoid ((<>))

-- | 'foldMap' equivalent for a 'Cons'-based structure
foldMap :: (Cons s s a a, Monoid m) => (a -> m) -> s -> m
foldMap f = go
  where
    go a =
      case uncons a of
        Nothing -> mempty
        Just (x, xs) -> f x <> go xs

-- | 'traverse' equivalent for a 'Cons'-based structure
traverse
  :: ( AsEmpty s, Cons s s a a
     , AsEmpty t, Cons t t b b
     , Applicative f
     )
  => (a -> f b) -> s -> f t
traverse f = go
  where
    go a =
      case uncons a of
        Nothing -> pure Empty
        Just (x, xs) -> cons <$> f x <*> go xs
