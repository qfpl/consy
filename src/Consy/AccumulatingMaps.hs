{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.AccumulatingMaps
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , mapAccumL
  , mapAccumR
  )
where

import Control.Applicative (Applicative(..))
import Control.Lens.Cons (Cons)
import Control.Lens.Empty (AsEmpty)
import Consy.Traversable (traverse)
import Data.Function ((.), ($), flip)
import Data.Functor (Functor(..))

import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector


{-# inline [2] mapAccumL #-}
-- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL
  :: (AsEmpty s, Cons s s b b, AsEmpty t, Cons t t c c)
  => (a -> b -> (a, c))
  -> a
  -> s
  -> (a, t)
mapAccumL = \f s t -> runStateL (traverse (StateL . flip f) t) s

{-# rules
"cons mapAccumL text" [~2]
    mapAccumL @Data.Text.Text = Data.Text.mapAccumL

"cons mapAccumL text eta" [~2]
    forall f s t.
    mapAccumL @Data.Text.Text f s t = Data.Text.mapAccumL f s t

"cons mapAccumL text lazy" [~2]
    mapAccumL @Data.Text.Lazy.Text = Data.Text.Lazy.mapAccumL

"cons mapAccumL text lazy eta" [~2]
    forall f s t.
    mapAccumL @Data.Text.Lazy.Text f s t = Data.Text.Lazy.mapAccumL f s t

"cons mapAccumL vector" [~2]
    mapAccumL @(Data.Vector.Vector _) = Data.List.mapAccumL

"cons mapAccumL vector eta" [~2]
    forall f s t.
    mapAccumL @(Data.Vector.Vector _) f s t = Data.List.mapAccumL f s t
#-}


{-# inline [2] mapAccumR #-}
-- mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR
  :: (AsEmpty s, Cons s s b b, AsEmpty t, Cons t t c c)
  => (a -> b -> (a, c))
  -> a
  -> s
  -> (a, t)
mapAccumR = \f s t -> runStateR (traverse (StateR . flip f) t) s

{-# rules
"cons mapAccumR text" [~2]
    mapAccumR @Data.Text.Text = Data.Text.mapAccumR

"cons mapAccumR text eta" [~2]
    forall f s t.
    mapAccumR @Data.Text.Text f s t = Data.Text.mapAccumR f s t

"cons mapAccumR text lazy" [~2]
    mapAccumR @Data.Text.Lazy.Text = Data.Text.Lazy.mapAccumR

"cons mapAccumR text lazy eta" [~2]
    forall f s t.
    mapAccumR @Data.Text.Lazy.Text f s t = Data.Text.Lazy.mapAccumR f s t

"cons mapAccumR vector" [~2]
    mapAccumR @(Data.Vector.Vector _) = Data.List.mapAccumR

"cons mapAccumR vector eta" [~2]
    forall f s t.
    mapAccumR @(Data.Vector.Vector _) f s t = Data.List.mapAccumR f s t
#-}

-- from Data.Functor.Utils, which is hidden in base

newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
    fmap f (StateL k) = StateL $ \ s -> let (s', v) = k s in (s', f v)

instance Applicative (StateL s) where
    pure x = StateL (\ s -> (s, x))
    StateL kf <*> StateL kv = StateL $ \ s ->
        let (s', f) = kf s
            (s'', v) = kv s'
        in (s'', f v)
    liftA2 f (StateL kx) (StateL ky) = StateL $ \s ->
        let (s', x) = kx s
            (s'', y) = ky s'
        in (s'', f x y)

newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where
    fmap f (StateR k) = StateR $ \ s -> let (s', v) = k s in (s', f v)

instance Applicative (StateR s) where
    pure x = StateR (\ s -> (s, x))
    StateR kf <*> StateR kv = StateR $ \ s ->
        let (s', v) = kv s
            (s'', f) = kf s'
        in (s'', f v)
    liftA2 f (StateR kx) (StateR ky) = StateR $ \ s ->
        let (s', y) = ky s
            (s'', x) = kx s'
        in (s'', f x y)
