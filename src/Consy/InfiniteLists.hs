{-# language NoImplicitPrelude #-}
{-# language TypeApplications #-}
module Consy.InfiniteLists
  ( module Control.Lens.Cons
  , module Control.Lens.Empty
  , iterate
  , iterate'
  , repeat
  , replicate
  , cycle
  )
where

import Control.Lens.Cons
import Control.Lens.Empty
import Data.Char (Char)
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Base (seq)
import GHC.List (errorEmptyList)
import GHC.Real (fromIntegral)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector

import Consy.Basic (append)
import Consy.Folds (build)
import Consy.ExtractingSublists (take)


{-# noinline [1] iterate #-}
-- iterate :: (a -> a) -> a -> [a]
iterate :: (AsEmpty s, Cons s s a a) => (a -> a) -> a -> s
iterate f = go
  where go a = a `cons` go (f a)

{-# inline [0] iterateFB #-}
iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x0 = go x0
  where
    go x = x `c` go (f x)

{-# rules
"cons iterate" [~1]
    forall f x.
    iterate f x = build (\c _n -> iterateFB c f x)
"cons iterateFB" [1]
    iterateFB (:) = iterate

"cons iterate ltext" [~1]
  iterate @Data.Text.Lazy.Text @Char =
    \f -> Data.Text.Lazy.iterate f
"cons iterate ltext eta" [~1]
    forall f x.
    iterate @Data.Text.Lazy.Text @Char f x =
      Data.Text.Lazy.iterate f x

"cons iterate bslazy" [~1]
  iterate @LBS.ByteString @Word8 =
    \f -> LBS.iterate f
"cons iterate bslazy eta" [~1]
    forall f x.
    iterate @LBS.ByteString @Word8 f x = LBS.iterate f x
#-}


{-# noinline [1] iterate' #-}
-- iterate' :: (a -> a) -> a -> [a]
iterate' :: (AsEmpty s, Cons s s a a) => (a -> a) -> a -> s
iterate' f = go
  where
    go x =
      let
        x' = f x
      in
        x' `seq` (x `cons` go x')

{-# inline [0] iterate'FB #-}
iterate'FB :: (a -> b -> b) -> (a -> a) -> a -> b
iterate'FB c f x0 = go x0
  where
    go x =
      let x' = f x
      in x' `seq` (x `c` go x')

{-# rules
"cons iterate'" [~1]
    forall f x.
    iterate' f x = build (\c _n -> iterate'FB c f x)
"cons iterate'FB"  [1]
    iterate'FB (:) = iterate'
#-}


{-# inline [0] repeat #-}
-- repeat :: a -> [a]
repeat :: (AsEmpty s, Cons s s a a) => a -> s
repeat x = xs
  where
    xs = x `cons` xs

{-# inline [0] repeatFB #-}
repeatFB :: (a -> b -> b) -> b -> a -> b
repeatFB c _ x = xs
  where
    xs = x `c` xs

{-# rules
"cons repeat" [~1]
    forall x.
    repeat x = build (\c n -> repeatFB c n x)
"cons repeatFB" [1]
    repeatFB (:) [] = repeat

"cons repeat ltext"
    repeat @Data.Text.Lazy.Text = Data.Text.Lazy.repeat
"cons repeat ltext eta"
    forall x.
    repeat @Data.Text.Lazy.Text @Char x =
      Data.Text.Lazy.repeat x

"cons repeat bslazy"
    repeat @LBS.ByteString = LBS.repeat
"cons repeat bslazy eta"
    forall x.
    repeat @LBS.ByteString @Word8 x = LBS.repeat x
#-}


{-# inline [2] replicate #-}
-- replicate :: Int -> a -> [a]
replicate :: (AsEmpty s, Cons s s a a) => Int -> a -> s
replicate = \n x -> take n (repeat x)

{-# rules
"cons replicate text" [~2]
  replicate @Text @Char = \n x -> Data.Text.replicate n (Data.Text.singleton x)
"cons replicate text eta" [~2] forall n x.
  replicate @Text @Char n x = Data.Text.replicate n (Data.Text.singleton x)

"cons replicate ltext" [~2]
  replicate @Data.Text.Lazy.Text @Char =
    \n x -> Data.Text.Lazy.replicate (fromIntegral n) (Data.Text.Lazy.singleton x)
"cons replicate ltext eta" [~2] forall n x.
  replicate @Data.Text.Lazy.Text @Char n x =
    Data.Text.Lazy.replicate (fromIntegral n) (Data.Text.Lazy.singleton x)

"cons replicate vector" [~2] replicate @(Vector _) = Data.Vector.replicate
"cons replicate vector eta" [~2] forall n x.
  replicate @(Vector _) n x = Data.Vector.replicate n x

"cons replicate bs" [~2]
  replicate @BS.ByteString = \n x -> BS.replicate (fromIntegral n) x
"cons replicate bs eta" [~2] forall n x.
  replicate @BS.ByteString n x = BS.replicate (fromIntegral n) x

"cons replicate bslazy" [~2]
  replicate @LBS.ByteString = \n x -> LBS.replicate (fromIntegral n) x
"cons replicate bslazy eta" [~2] forall n x.
  replicate @LBS.ByteString n x = LBS.replicate (fromIntegral n) x
#-}


{-# inline [2] cycle #-}
-- cycle :: [a] -> [a]
cycle :: (Cons s s a a) => s -> s
cycle = \s ->
  case uncons s of
    Nothing -> errorEmptyList "cycle"
    Just _ -> xs where xs = s `append` xs

{-# rules
"cons cycle ltext" [~2]
    cycle @Data.Text.Lazy.Text = Data.Text.Lazy.cycle
"cons cycle ltext eta" [~2]
    forall x.
    cycle @Data.Text.Lazy.Text x = Data.Text.Lazy.cycle x

"cons cycle bslazy" [~2]
    cycle @LBS.ByteString = LBS.cycle
"cons cycle bslazy eta" [~2]
    forall x.
    cycle @LBS.ByteString x = LBS.cycle x
#-}
