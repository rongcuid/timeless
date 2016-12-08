-- |
-- Module:     Control.Wire
-- Copyright:  (c) Ertugrul Soeylemez, 2013
--             (c) Rongcui Dong, 2015
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless
  ( -- * Reexports
    module FRP.Timeless.Run

  -- * High level FRP
  , Stream
  , Cell
  , StreamCell
  -- * FRP Primitives
  , arrS
  , neverS
  , onceS
  , sourceC
  , sinkC
  , sourceS
  , sinkS
  , mergeS
  , mergeSP
  , hold
  , filterS
  , snapshot
  , sample
  , state
  , zipS
  , zipS3
  , zipS4
  , zipS5
  , zipS6
  , zipS7
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
import Control.Monad.Zip
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

type StreamCell a b = Signal IO (Maybe a) b

-- * FRP Primitives

arrS :: (a -> b) -> Stream a b
arrS = arr . fmap

-- | A 'StreamSource' that never fires
neverS :: StreamSource b
neverS = mkConst $ Just Nothing

-- | A 'StreamSource' that fires only ones
onceS :: b -> StreamSource b
onceS b = mkSF $ \_ -> (Just b, neverS)

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
mergeS :: ((a,a) -> a) -> Signal IO (Maybe a, Maybe a) (Maybe a)
mergeS f = mkSF_ g
  where
    g (Just a, Nothing) = Just a
    g (Nothing, Just b) = Just b
    g (Nothing, Nothing) = Nothing
    g (Just a, Just b) = Just $ f (a,b)

-- | Merges two 'Stream' with precedence to first. P stands for Priority
mergeSP = mergeS fst

-- | Holds a discrete value to be continuous. An initial value must be given
hold :: a -> StreamCell a a
hold a0 = mkSW_ a0 $ \a ma ->
  case ma of
    Just a' -> a'
    Nothing -> a

-- | Filters stream of event.
-- TODO: In future, might implement 'Foldable'
filterS :: (a -> Bool) -> Stream a a
filterS pred = mkSF_ $ \ma -> do
  a <- ma
  if (pred a) then ma
              else Nothing

-- | Takes a snapshot of b when an event a comes. Meanwhile, transform the
-- 'Stream' with the 'Cell' value
snapshot :: ((a,b) -> c) -> Signal IO (Maybe a, b) (Maybe c)
snapshot f = mkSF_ $ \(ma, b) ->
  case ma of
    Just a -> Just $ f (a,b)
    Nothing -> Nothing

-- | This conviniently just samples a Cell
sample :: Signal IO (Maybe a, b) (Maybe b)
sample = snapshot snd

-- | A state block, updates on event. Note that this can be
-- constructed with 'Signal' directly, but we are using primitives
-- instead, for easy reasoning
state :: s -> ((a, s) -> s) -> StreamCell a s
state s0 update = loop $ proc (ma, s) -> do
  sDelay <- delay s0 -< s
  s' <- hold s0 <<< snapshot update -< (ma, sDelay)
  returnA -< (s', s')

mzip3 (ma, mb, mc) = do
    a <- ma
    b <- mb
    c <- mc
    return (a, b, c)

mzip4 (ma, mb, mc, md) = do
    a <- ma
    b <- mb
    c <- mc
    d <- md
    return (a, b, c, d)

mzip5 (ma, mb, mc, md, me) = do
    a <- ma
    b <- mb
    c <- mc
    d <- md
    e <- me
    return (a, b, c, d, e)

mzip6 (ma, mb, mc, md, me, mf) = do
    a <- ma
    b <- mb
    c <- mc
    d <- md
    e <- me
    f <- mf
    return (a, b, c, d, e, f)

mzip7 (ma, mb, mc, md, me, mf, mg) = do
    a <- ma
    b <- mb
    c <- mc
    d <- md
    e <- me
    f <- mf
    g <- mg
    return (a, b, c, d, e, f, g)

zipS :: Signal m (Maybe a, Maybe b) (Maybe (a, b))
zipS = SArr $ fmap $ uncurry mzip

zipS3 :: Signal m (Maybe a, Maybe b, Maybe c) (Maybe (a, b, c))
zipS3 = SArr $ fmap mzip3
zipS4 :: Signal m (Maybe a, Maybe b, Maybe c, Maybe d) (Maybe (a, b, c, d))
zipS4 = SArr $ fmap mzip4
zipS5 :: Signal m (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e) (Maybe (a, b, c, d, e))
zipS5 = SArr $ fmap mzip5
zipS6 :: Signal m (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f) (Maybe (a, b, c, d, e, f))
zipS6 = SArr $ fmap mzip6
zipS7 :: Signal m (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g) (Maybe (a, b, c, d, e, f, g))
zipS7 = SArr $ fmap mzip7
