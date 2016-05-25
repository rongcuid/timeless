{-|
Module      : Signal
Description : Core Signal type and runner
Copyright   : (c) Rongcui Dong, 2016
License     : BSD3
Maintainer  : rongcuid@outlook.com
Stability   : experimental

This module includes the core 'Signal' type, its compiler, and runner. To easily
create signals, import module "Timeless" and use functions in module "Factory".
Also, most explanations are written in the "Timeless" module.

-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}


module Signal where

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Data.Monoid
import           Prelude          hiding (id, (.))

data Complexity = Complexity {
complexityNS   :: Integer -- ^ Number of primitive 'Signals'
, complexityNC :: Integer -- ^ Number of compositions
, complexityNP :: Integer -- ^ Number of parallels/splits '(:***:)'
, complexityNF :: Integer -- ^ Number of fan outs '(:&&&:)'
} deriving (Eq)

instance Monoid Complexity where
  mempty = Complexity 0 0 0 0
  mappend (Complexity a b c d) (Complexity a' b' c' d') =
    Complexity (a+a') (b+b') (c+c') (d+d')

infixr 1 :>:
infixr 1 :<:
data Signal m a b where
  SEmpty :: Signal m a b -- ^ A Signal that never produces anything
  SId :: Signal m a a -- ^ An identity Signal
  SPure :: (a -> b) -> Signal m a b -- ^ A Signal of pure function
  -- SArr :: Monad m => (a -> m b) -> Signal m a b -- ^ A Signal of monadic action
  SCtrl :: Monad m => (Maybe a -> m (Maybe b)) -> Signal m a b -- ^ A controller
  SGen :: Monad m => s -> ((a, s) -> m (b, s)) -> Signal m a b -- ^ A Generator with state
  (:>:) :: Signal m a c -> Signal m c b -> Signal m a b -- ^ Chain Signals
  (:<:) :: Signal m c b -> Signal m a c -> Signal m a b -- ^ Chain the other way
  (:***:) :: Signal m a b -> Signal m a' b' -> Signal m (a,a') (b,b')
  (:&&&:) :: Signal m a b -> Signal m a b' -> Signal m a (b,b')

instance Monad m => Category (Signal m) where
  id = SId
  s1 . s2 = s1 :<: s2

instance Monad m => Arrow (Signal m) where
  arr = SPure
  first = (:***:SId)
  second = (SId:***:)
  (***) = (:***:)
  (&&&) = (:&&&:)

-- | Simplifies a signal network for one step
simplify :: Monad m => Signal m a b -> Signal m a b
simplify (SEmpty :>: sig) = SEmpty
simplify (sig :>: SEmpty) = SEmpty
-- * Parallel
-- ** first
simplify (s :***: SId) = first' s
-- ** second
simplify (SId :***: s) = second' s
simplify (s1 :***: s2) = first' s1 >>> second' s2
simplify (s1 :&&&: s2) = arr (\b->(b,b)) >>> s1 *** s2
-- * Series
-- ** Identity
simplify (SId :>: sig) = sig
simplify (sig :>: SId) = sig
-- ** Composition
simplify (SPure f1 :>: SPure f2) = SPure (f1 >>> f2)
-- simplify (SPure f1 :>: SArr f2) = SArr (f1 >>> return >=> f2)
-- simplify (SArr f1 :>: SPure f2) = SArr (f1 >>> fmap f2)
-- simplify (SArr f1 :>: SArr f2) = SArr (f1 >=> f2)
simplify (SPure f1 :>: SGen s f2) = SGen s (\(a,s)->f2 (f1 a, s))
simplify (SGen s f1 :>: SPure f2) = SGen s $ \as -> do
  (c, s') <- f1 as
  return (f2 c, s')
simplify (SCtrl f1 :>: SCtrl f2) = SCtrl (f1 >=> f2)
simplify (s1 :<: s2) = simplify (s2 :>: s1)
simplify s = s

-- | A version of 'first' which simplifies the signal
first' :: Signal m a b -> Signal m (a,c) (b,c)
first' SEmpty = SEmpty :***: SId
first' SId = SId
first' (SPure f) = SPure $ first f
-- first' (SArr f) = SArr . runKleisli . first . Kleisli $ f
first' (SCtrl f) = SCtrl $ \mad -> case mad of
  Just (a,d) -> fmap (,d) <$> f (Just a)
  Nothing -> return Nothing
first' (SGen s f) = SGen s $ \((a,c),s') -> do
  (b, s) <- f (a,s')
  return ((b,c), s)
first' (s1 :<: s2) = first' $ s2 :>: s1
first' (s1 :>: s2) = first' s1 :>: first' s2

-- | A version of 'second' which simplifies the signal
second' s = let swap ~(x,y)=(y,x) in arr swap >>> first' s >>> arr swap

-- | Gets the current complexity of a 'Signal'
complexity :: Signal m a b -> Complexity
complexity (s2 :<: s1) = complexity (s1 :>: s2)
complexity (s1 :>: s2) = complexity s1 <> complexity s2 <> Complexity 0 1 0 0
complexity (s1 :***: s2) = complexity s1 <> complexity s2 <> Complexity 0 0 1 0
complexity (s1 :&&&: s2) = complexity s1 <> complexity s2 <> Complexity 0 0 0 1
complexity _ = Complexity 1 0 0 0

-- | Simplifies the 'Signal' to the simplest form
compile :: Monad m => Signal m a b -> Signal m a b
compile s' = let s = simplify s' in if complexity s == complexity s'
  then s else compile s

-- | Steps a signal with an input
stepSignal :: Monad m => Signal m a b -> Maybe a -> m (Maybe b, Signal m a b)
stepSignal SEmpty _ = return (Nothing, SEmpty)
stepSignal sig Nothing = return (Nothing, sig)
stepSignal SId a = return (a, SId)
-- stepSignal sig@(SArr f) (Just a) = (,sig) <$> (fmap Just . f) a
stepSignal sig@(SCtrl f) a = (,sig) <$> f a
stepSignal sig@(SGen s' f) (Just a) = do
  (b,s) <- f (a, s')
  return (Just b, SGen s f)
stepSignal (s1' :>: s2') a = do
  (mx, s1) <- stepSignal s1' a
  (mb, s2) <- stepSignal s2' mx
  return (mb, s1 :>: s2)
stepSignal (s2 :<: s1) a = stepSignal (s1 :>: s2) a
stepSignal (s1' :***: s2') (Just (a,a')) = do
  (mb, s1) <- stepSignal s1' $ Just a
  (mb', s2) <- stepSignal s2' $ Just a'
  return ((,) <$> mb <*> mb', s1 :***: s2)
stepSignal (s1' :&&&: s2') a = do
  (mb, s1) <- stepSignal s1' a
  (mb', s2) <- stepSignal s2' a
  return ((,) <$> mb <*> mb', s1 :&&&: s2)

-- | Runs a signal with a monadic action as input
runSignal :: Monad m => Signal m a b -> m a -> m (Signal m a b)
runSignal s' ma = do
  a <- ma
  (mb, s) <- stepSignal s' (Just a)
  case mb of
    Just b -> runSignal s ma
    Nothing -> return s

runBox :: Monad m => Signal m () () -> m ()
runBox s' = void $ runSignal s' (return ())
