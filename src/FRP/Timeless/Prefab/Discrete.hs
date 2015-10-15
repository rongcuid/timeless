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

-- | Rising edge filter. Creates an impulse at rising edge
rising :: (Monad m) =>
          Bool -- ^ Initial value
       -> Signal s m Bool Bool
rising b0 = mkPWN $ f b0
    where
      f False b = (b, rising b)
      f True b = (False, rising b)

-- | Falling edge filter. Creates an impulse at falling edge
falling :: (Monad m) =>
          Bool -- ^ Initial value
       -> Signal s m Bool Bool
falling b0 = mkPWN $ f b0
    where
      f False b = (False, rising b)
      f True b = (not b, rising b)

-- | Edge filter. Creates an impulse at edge
edge :: (Monad m) =>
        Bool -- ^ Initial value
     -> Signal s m Bool Bool
edge b0 = proc b -> do
  b'1 <- rising b0 -< b
  b'2 <- falling b0 -< b
  returnA -< b'1 || b'2

-- | A Set-Reset latch, with the first input set, second input
-- reset. Current output value has higher priority (Prefer lazy!)
latch :: (Monad m) =>
         Bool -- ^ Initial value
      -> Signal s m (Bool, Bool) Bool
latch b0 = mkPWN $ f b0
    where
      f False (True, False) = (True, latch True)
      f True (False, True) = (False, latch False)
      f b0 (_, _) = (b0, latch b0)

-- | A set-latch whose initial value is False, but turns True and holds
-- when its input becomes true
latchS :: (Monad m) => Signal s m Bool Bool
latchS = proc s -> do
            returnA <<< latch False -< (s, False)

-- | A reset-latch whose initial value is True, but turns False and holds
-- when its input becomes false
latchR :: (Monad m) => Signal s m Bool Bool
latchR = proc s -> do
            returnA <<< latch True -< (False, s)
