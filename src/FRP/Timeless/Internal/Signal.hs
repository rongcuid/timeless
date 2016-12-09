-- |
-- Module:     FRP.Timeless.Signal
-- Copyright:  (c) Ertugrul Soeylemez, 2013
--                 Rongcui Dong, 2015
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Internal.Signal
    (-- * Signal
     Signal(..)
    , stepSignal

    -- * Utilities
    , lstrict
    )
    where

import Prelude hiding ((.),id)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Data.Monoid
import Control.Category

data Signal m a b where
  SId :: Signal m a a
  SConst :: Maybe b -> Signal m a b
  SArr :: (Maybe a -> Maybe b) -> Signal m a b
  SPure :: (Maybe a -> (Maybe b, Signal m a b)) -> Signal m a b
  SGen :: (Monad m) =>
    (Maybe a -> m (Maybe b, Signal m a b)) -> Signal m a b


instance (Monad m) => Category (Signal m) where
    id = SId
    s2 . s1 = SGen $ \mx0 -> do
                (mx1, s1') <- stepSignal s1 mx0
                (mx2, s2') <- stepSignal s2 mx1
                mx2 `seq` return (mx2, s2'. s1')

instance (Monad m) => Arrow (Signal m) where
    arr f = SArr (fmap f)
    first s = SGen f
        where
          f mxy =
              let mx = fst <$> mxy
                  my = snd <$> mxy
                  mmxs' = stepSignal s mx in
                liftM (g my) mmxs'
          g my (mx', s') =
              let mx'y = (,) <$> mx' <*> my in
              lstrict (mx'y, first s')

instance (Monad m) => ArrowChoice (Signal m) where
  left s =
    SGen $ \mmx ->
    liftM (fmap Left ***! left) . stepSignal s $
    case mmx of
      Just (Left x) -> Just x
      Just (Right x) -> Nothing
      Nothing -> Nothing

  right s =
    SGen $ \mmx ->
    liftM (fmap Right ***! right) . stepSignal s $
    case mmx of
      Just (Left x) -> Nothing
      Just (Right x) -> Just x
      Nothing -> Nothing

  sl +++ sr =
    SGen $ \mmx ->
    case mmx of
    Just (Left x) -> do
      liftM2 (\ (mx,sl')(_,sr') -> lstrict (fmap Left mx, sl' +++ sr'))
        (stepSignal sl (Just x)) (stepSignal sr Nothing)
    Just (Right x) -> do
      liftM2 (\ (_,sl')(mx,sr') -> lstrict (fmap Right mx, sl' +++ sr'))
        (stepSignal sl Nothing) (stepSignal sr (Just x))
    Nothing ->
      liftM2 (\ (_,sl')(_,sr') -> lstrict (Nothing, sl' +++ sr'))
        (stepSignal sl Nothing) (stepSignal sr Nothing)

  sl ||| sr =
    SGen $ \mmx ->
    case mmx of
    Just (Left x) -> do
      liftM2 (\(mx,sl')(_,sr') -> lstrict (mx, sl' ||| sr'))
        (stepSignal sl (Just x)) (stepSignal sr Nothing)
    Just (Right x) -> do
      liftM2 (\(_,sl')(mx,sr') -> lstrict (mx, sl' ||| sr'))
        (stepSignal sl Nothing) (stepSignal sr (Just x))
    Nothing -> do
      liftM2 (\(_,sl')(_,sr') -> lstrict (Nothing, sl' ||| sr'))
        (stepSignal sl Nothing) (stepSignal sr Nothing)

instance (MonadFix m) => ArrowLoop (Signal m) where
  loop s =
    SGen $ \mx ->
      liftM (fmap fst ***! loop) .
      mfix $ \ ~(mx',_) ->
        let d | Just (_,d) <- mx' = d
              | otherwise = error "Feedback broken by inhibition"
        in stepSignal s (fmap (,d) mx)

instance (Monad m) => Functor (Signal m a) where
    fmap f SId = SArr $ fmap f
    fmap f (SConst mx) = SConst $ fmap f mx
    fmap f (SArr g) = SArr $ fmap f . g
    fmap f (SPure g) = SPure $ (fmap f ***! fmap f) . g

instance (Monad m) => Applicative (Signal m a) where
    pure = SConst . Just
    sf <*> sx =
        SGen $ \mx ->
        liftM2 (\(mf, sf) (mx, sx) -> lstrict (mf <*> mx, sf <*> sx))
        (stepSignal sf mx)
        (stepSignal sx mx)



-- | Steps a signal in certain time step
stepSignal :: (Monad m) =>
              Signal m a b
           -- ^ Signal to be stepped
           -> Maybe a
           -- ^ Input
           -- | Stateful output
           -> m (Maybe b, Signal m a b)
stepSignal s@(SId) mx = return $ lstrict (mx, s)
stepSignal s@(SConst mx) _ = return $ lstrict (mx, s)
stepSignal s@(SArr f) mx = return $ lstrict (f mx, s)
stepSignal s@(SPure f) mx = return $ lstrict (f mx)
stepSignal s@(SGen f) mx = lstrict <$> f mx

-- | Left-strict version of '&&&' for functions.
(&&&!) :: (a -> b) -> (a -> c) -> (a -> (b, c))
(&&&!) f g x' =
    let (x, y) = (f x', g x')
    in x `seq` (x, y)


-- | Left-strict version of '***' for functions.
(***!) :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
(***!) f g (x', y') =
    let (x, y) = (f x', g y')
    in x `seq` (x, y)

-- | Left strict tuple
lstrict :: (a,b) -> (a,b)
lstrict (x,y) = x `seq` (x,y)

