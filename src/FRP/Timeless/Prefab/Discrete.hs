-- |
-- Module:     FRP.Timeless.Prefab.Discrete
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Prefab.Discrete
    where

import Control.Arrow
import Control.Applicative
import Data.Monoid 
import Control.Monad
import Control.Monad.IO.Class

import FRP.Timeless.Signal
import FRP.Timeless.Prefab.Primitive

-- | Produces output for a several sample periods, then inhibits.
--
-- Typical usage:
--
-- > () `occursFor` 1 >>> <some IO actions> >>> snapOnce
--
-- The example above will perform the IO action once and then hold the
-- result forever
occursFor :: (Monad m) => b -- ^ Constant Output
          -> Int -- ^ Number of sample periods
          -> Signal s m a b
occursFor b n = mkPW_ (\_ -> b) >>> inhibitsAfter n

-- | Takes the snapshot of the value when signal is activated,
-- and then holds value forever
--
-- Typical usage:
--
-- > () `occursFor` 1 >>> <some IO actions> >>> snapOnce
--
-- The example above will perform the IO action once and then hold the
-- result forever
snapOnce :: (Monad m) => Signal s m a a
snapOnce = SGen $ \_ ma -> return (ma, SConst ma)

-- | Acts as identity for a several sample periods, then inhibits.
inhibitsAfter :: Int -> Signal s m a a
inhibitsAfter n
    | n == 0 = mkEmpty
    | n > 0 = mkPureN $ \a -> (Just a, inhibitsAfter $ n-1)
    | otherwise = error "[ERROR] inhibitsAfter: Nothing will inhibit in the past!"

-- | Runs a signal once and hold the result forever.
--
-- It is a combination of 'inhibitsAfter' and 'snapOnce'
runAndHold :: (Monad m) =>
              Signal s m a b
           -> Signal s m a b
runAndHold sig = inhibitsAfter 1 >>> sig >>> snapOnce
