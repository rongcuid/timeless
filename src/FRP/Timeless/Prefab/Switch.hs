-- | 
-- Module:     FRP.Timeless.Prefab.Switch
-- Copyright:  (c) Rongcui Dong, 2015
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Prefab.Switch
    where

import Control.Arrow
import Control.Applicative
import Data.Monoid 
import Control.Monad
import Control.Monad.IO.Class

import FRP.Timeless.Signal
import FRP.Timeless.Session
import FRP.Timeless.Prefab.Primitive

(-->) :: (Monad m) =>
         Signal s m a b
      -> Signal s m a b
      -> Signal s m a b
s1 --> s2 = 
