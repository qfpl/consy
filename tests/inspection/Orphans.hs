module Orphans where

import Control.Applicative
import Control.Lens.Empty
import Control.Lens.Prism

instance AsEmpty (ZipList a) where
  _Empty = nearly (ZipList []) (null . getZipList)
  {-# inline _Empty #-}
