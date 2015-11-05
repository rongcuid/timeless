-- |
-- Module:     FRP.Timeless.Prefab.Utils
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Prefab.Utils
    where

import Control.Arrow
import Control.Applicative
import Data.Monoid 
import Control.Monad
import Control.Monad.IO.Class

import FRP.Timeless.Signal
import FRP.Timeless.Session
import FRP.Timeless.Prefab.Primitive


-- | Local time starting from zero.

time :: (HasTime t s) => Signal s m a t
time = timeFrom 0


-- | Local time starting from zero, converted to your favorite
-- fractional type.

timeF :: (Fractional b, HasTime t s, Monad m) => Signal s m a b
timeF = fmap realToFrac time


-- | Local time starting from the given value.

timeFrom :: (HasTime t s) => t -> Signal s m a t
timeFrom t' =
    mkSF $ \ds _ ->
        let t = t' + dtime ds
        in lstrict (t, timeFrom t)


-- | A signal for easy debugging in arrow environment
sDebug :: (MonadIO m) => Signal s m String ()
sDebug = mkKleisli_ $ \s -> liftIO $ putStr s
      
