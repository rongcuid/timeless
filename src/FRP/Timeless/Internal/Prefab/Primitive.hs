-- |
-- Module:     FRP.Timeless.Prefab.Primitive
-- Copyright:  (c) Ertugrul Soeylemez, 2013
--                 Rongcui Dong, 2015
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>
--
-- This module contains the lowest level primitives of timeless,
-- directly working with the Signal arrow. Try not to use them
-- as they tend to be very hard to grasp. Use them only when
-- building a new FRP framework.
--
-- Understand that using 'Signal' directly can be difficult to reason about

module FRP.Timeless.Internal.Prefab.Primitive
    (
      -- * Basic Signals
      mkEmpty
    , mkId
    , mkConst
    , mkPure
    , mkGen
      -- * Pure Signals
      -- ** Signals
    , mkPure_
    , mkSF
    , mkSF_
    , mkSW_
    -- * Monadic Signals
    , mkGen_

    -- * Kleisli Signals
    , mkKleisli_
    , mkSK_
    , mkConstM
    , mkActM
    -- * Special signals
    , delay
    )
    where

import Control.Arrow
import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class

import FRP.Timeless.Internal.Signal

-- | Make a pure stateful signal from given signal function
mkSF :: (a -> (b, Signal m a b)) -> Signal m a b
mkSF f = mkPure (lstrict . first (Just) . (f))
-- first (Just) has type (a, b) -> (Maybe a, b)

-- | Make a pure stateless signal from given signal function
mkSF_ :: (a -> b) -> Signal m a b
mkSF_ = SArr . fmap

-- | Make a stateful wire from chained state transition
-- function. Notice that the output will always be the new value
mkSW_ :: b -> (b -> a -> b) -> Signal m a b
mkSW_ b0 f = mkSF $ g b0
    where
      g b0 x = let b1 = f b0 x in
                   lstrict (b1, mkSW_ b1 f)

-- | Make a signal that inhibits forever
mkEmpty :: Signal m a b
mkEmpty = SConst Nothing

-- | The Identity Signal
mkId :: Signal m a a
mkId = SId

-- | Make a constant Signal
mkConst :: Maybe b -> Signal m a b
mkConst = SConst

-- | Make a pure stateful signal from given transition function
mkPure :: (a -> (Maybe b, Signal m a b)) -> Signal m a b
mkPure f =
  SPure $ \mx ->
  case mx of
    Just x -> lstrict $ f x
    Nothing -> (Nothing, mkPure f)

-- | Make a pure stateless signal from given function
mkPure_ :: (a -> (Maybe b)) -> Signal m a b
mkPure_ f = go
    where
      go = SPure $ \mx ->
           case mx of
             Just x -> lstrict (f x, go)
                 -- From (m (Maybe b)) to (m (Maybe b, Signal m a b))
             Nothing -> (Nothing, go)

-- | Make a stateful signal from given (Monadic) transition function
mkGen :: (Monad m) => (a -> m (Maybe b, Signal m a b)) -> Signal m a b
mkGen f = SGen $ \ mx ->
           case mx of
             Just x -> liftM lstrict $ f x
             Nothing -> return (Nothing, mkGen f)

-- | Make a stateless signal from given function
mkGen_ :: (Monad m) => (a -> m (Maybe b)) -> Signal m a b
mkGen_ f = loop
  where
    loop = SGen $ \mx ->
           case mx of
             Just x ->
                 let mmx' = f x in
                     liftM (lstrict . (, loop)) mmx'
                 -- From (m (Maybe b)) to (m (Maybe b, Signal m a b))
             Nothing ->
               return (Nothing, loop)

-- | Make a stateless signal from Kleisli function
mkKleisli_ :: (Monad m) => (a -> m b) -> Signal m a b
mkKleisli_ f =  mkGen_ $ \x -> fmap Just (f x)

-- | Make a stateful signal from Kleisli function
mkSK_ :: (Monad m) => b -> (b -> a -> m b) -> Signal m a b
mkSK_ b f =  mkGen $ f'
    where
      f' a = do
        b' <- f b a
        return (Just b', mkSK_ b' f)

-- | Make a monadic constant wire
mkConstM :: (Monad m) => m b -> Signal m a b
mkConstM b = mkKleisli_ $ \_ -> b

-- | Make a monadic action wire, alias for mkConstM
mkActM :: (Monad m) => m b -> Signal m a b
mkActM = mkConstM


delay :: a -> Signal m a a
delay x' = mkSF $ \x -> (x', delay x)
