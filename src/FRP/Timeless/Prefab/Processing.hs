-- |
-- Module:     FRP.Timeless.Prefab.Processing
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Prefab.Processing
    (
      sample
    )
    where

import Control.Arrow
import Control.Applicative
import Data.Monoid 
import Control.Monad
import Control.Monad.IO.Class

import FRP.Timeless.Signal
import FRP.Timeless.Prefab.Primitive

-- | Takes a sample of second input when the first input becomes
-- True. First snapshot taken at local time 0, i.e. on construction
sample :: (Monad m) => Signal s m (Bool, a) a
sample = mkPWN f
    where
      -- | First snapshot taken on local time 0 (On construction)
      f (_, a) = (a, mkPWN $ f' a)
      -- | Later snapshots taken when predicate becomes true
      f' a (False, _) = (a, mkPWN $ f' a)
      f' a (True, a') = (a', mkPWN $ f' a')

-- | Alias for 'sample'. Snapshot sounds more discrete
snapshot :: (Monad m) => Signal s m (Bool, a) a
snapshot = sample
