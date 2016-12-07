-- |
-- Module:     Control.Wire
-- Copyright:  (c) Ertugrul Soeylemez, 2013
--             (c) Rongcui Dong, 2015
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless
  ( -- * Reexports
--       module Prelude
--     , module FRP.Timeless.Signal
--     , module FRP.Timeless.Prefab
      module FRP.Timeless.Run

    -- * High level FRP
    , Stream
    , Cell
    -- * FRP Primitives
     -- * External
    , module Control.Applicative
    , module Control.Arrow
    , module Control.Category
    , module Data.Time.Clock
    )
    where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow
import Control.Monad.Fix
import Control.Category
import FRP.Timeless.Internal.Signal
import FRP.Timeless.Internal.Prefab
import FRP.Timeless.Run
import Data.Time.Clock

-- Stream and Cell

-- | A Stream of discrete events.
type Stream a b = Signal IO (Maybe a) (Maybe b)

-- | A Source of discrete event
type StreamSource b = Signal IO () (Maybe b)
-- | A Sink of discrete event
type StreamSink a = Signal IO (Maybe a) ()


-- | A Cell of continuous value.
--
-- Cells must not be inhibited
type Cell a b = Signal IO a b

-- | A Source of discrete event
type CellSource b = Signal IO () b
-- | A Sink of discrete event
type CellSink a = Signal IO a ()

-- * FRP Primitives

sourceC :: IO b -> CellSource b
sourceC = mkActM

sinkC :: (a -> IO ()) -> CellSink a
sinkC = mkKleisli_

sourceS :: IO (Maybe b) -> StreamSource b
sourceS = mkActM

sinkS :: (a -> IO ()) -> StreamSink a
sinkS f = mkKleisli_ $ \ma ->
  case ma of
    Just a -> f a >> return ()
    Nothing -> return ()

-- | Merges two 'Stream'. When simultaneous, use the merge function
mergeS :: ((a,a) -> a) -> Signal m (Maybe a, Maybe a) (Maybe a)
mergeS f = SArr (fmap g)
  where
    g (Just a, Nothing) = Just a
    g (Nothing, Just b) = Just b
    g (Nothing, Nothing) = Nothing
    g (Just a, Just b) = Just $ f (a,b)

-- | Merges two 'Stream' with precedence to first.
orElse = mergeS fst

-- | Holds a discrete value to be continuous. An initial value must be given
hold :: a -> Signal m (Maybe a) a
hold a0 = mkSW_ a0 $ \a ma ->
  case ma of
    Just a' -> a'
    Nothing -> a

-- | Filters stream of event.
-- TODO: In future, might implement 'Foldable'
filterS :: (a -> Bool) -> Stream a a
filterS pred = SArr . fmap $ \ma -> do
  a <- ma
  if (pred a) then ma
              else Nothing

-- | Takes a snapshot of b when an event a comes. Meanwhile, transform the
-- 'Stream' with the 'Cell' value
snapshot :: ((a,b) -> c) -> Signal m (Maybe a, b) (Maybe c)
snapshot f = SArr . fmap $ \(ma, b) ->
  case ma of
    Just a -> Just $ f (a,b)
    Nothing -> Nothing

-- | This conviniently just samples a Cell
sample = snapshot snd

-- | A state block, updates on event. Note that this can be
-- constructed with 'Signal' directly, but we are using primitives
-- instead, for easy reasoning
state :: MonadFix m => s' -> ((a, s') -> s') -> Signal m (Maybe a) s'
state s0 update = loop $ proc (ma, s) -> do
  sDelay <- delay s0 -< s
  s' <- hold s0 <<< snapshot update -< (ma, sDelay)
  returnA -< (s', s')
