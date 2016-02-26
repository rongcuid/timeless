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

-- | A simple switch which inhibits when input is False, and acts as constant when True
switch :: Monad m => b -> Signal s m Bool b
switch b = mkPure_ $ f
  where
    f True = Just b
    f False = Nothing

-- | This is the inverted version of 'switch'
iswitch :: Monad m => b -> Signal s m Bool b
iswitch b = (arr not) >>> (switch b)

-- | Immediately switches to the second signal after the first one inhibits, and
-- never switch back. Second signal is untouched until first signal
-- inhibits
(-->) :: (Monad m) =>
         Signal s m a b
      -> Signal s m a b
      -> Signal s m a b
s1 --> s2 =
    SGen $ \ds ma -> do
      (mb, s1') <- stepSignal s1 ds ma
      case mb of
        Just _ -> return (mb, s1' --> s2)
        Nothing -> stepSignal s2 ds ma
infixr 2 -->

-- | The flipped (-->)
(<--) :: (Monad m) =>
         Signal s m a b
      -> Signal s m a b
      -> Signal s m a b
s1 <-- s2 = s2 --> s1
infixl 2 <--

-- | Immediately switches to the second signal when it starts to produce
-- output, and never switches back. First signal is untouched after
-- second starts producing
(--<) :: (Monad m) =>
         Signal s m a b
      -> Signal s m a b
      -> Signal s m a b
s1 --< s2 =
    SGen $ \ds ma -> do
      (mb2, s2') <- stepSignal s2 ds ma
      case mb2 of
        Nothing -> do
                 (mb1, s1') <- stepSignal s1 ds ma
                 return (mb1, s1' --< s2')
        Just _ -> return (mb2, s2')
infixl 2 --<

-- | The flipped (--<)
(>--) :: (Monad m) =>
         Signal s m a b
      -> Signal s m a b
      -> Signal s m a b
s1 >-- s2 = s2 --< s1
infixr 2 >--
