-- |
-- Module:     FRP.Timeless.Prefab.Processing
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Prefab.Processing
    (
      sample
    , snapshot
    , integrateM
    , integrate
    , wait
    , waitWith
    , (<=>)
    , unless'
    , when'
    )
    where

import Control.Arrow
import Control.Applicative
import Data.Monoid 
import Control.Monad
import Control.Monad.IO.Class

import FRP.Timeless.Signal
import FRP.Timeless.Session
import FRP.Timeless.Prefab.Primitive

import qualified Debug.Trace as D

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

-- | Make an integration signal from a function that models the chage
integrateM :: (Monad m, Monoid b, HasTime t s) =>
                 b -- ^ Initial state
                 -> (s -> a -> b)
                 -- ^ The model, such as /dX/. 's' is delta session
                 -> Signal s m a b
integrateM b0 f = mkPW $ g b0
    where
      g b0 ds a = let db = f ds a
                      b1 = b0 <> db
                  in (b1, mkPW $ g b1)

-- | A numerical integration signal.
integrate :: (Monad m, Num a, HasTime t s) =>
             a -- ^ Initial state
          -> (s -> a -> a)
          -- ^ The model
          -> Signal s m a a
integrate a0 f = integrateM (Sum a0) (\s a -> Sum $ f s a) >>> arr getSum

-- | Acts as an identity signal for a certain time, then inhibits
wait :: (HasTime t s, Monad m) => Double -> Signal s m a a
wait t =
    mkPure $ \ds a ->
        let dt = realToFrac $ dtime ds in
        if | t < 0 -> (Nothing, mkEmpty)
           | otherwise -> (Just a, wait $ t - dt)

-- | Wait with a constant output
waitWith :: (HasTime t s, Monad m) => b -> Double -> Signal s m a b
waitWith b t = pure b >>> wait t

-- | A signal that outputs left side when input is True, and right
-- side when input is False
(<=>) :: Monad m =>
         Maybe b
      -> Maybe b
      -> Signal s m Bool b
mb1 <=> mb2 = mkPureN $ \case
            True -> (mb1, mb1 <=> mb2)
            False -> (mb2, mb1 <=> mb2)
infix 3 <=>

-- | A signal that acts like identity but inhibits when input satisfies condition.
unless' :: Monad m =>
           (a -> Bool) -- ^ Inhibit condition
        -> Signal s m a a
unless' pred = mkPureN $ \a ->
               if | pred a -> (Just a, unless' pred)
                  | otherwise -> (Nothing, unless' pred)

-- | A signal that inhibits but acts like identity when input satisfies condition.
when' :: Monad m =>
         (a -> Bool) -- ^ Activate condition
      -> Signal s m a a
when' pred = mkPureN $ \a ->
        if | pred a -> (Just a, when' pred)
           | otherwise -> (Nothing, when' pred)
