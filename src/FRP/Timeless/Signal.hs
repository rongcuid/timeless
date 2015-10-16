-- |
-- Module:     FRP.Timeless.Signal
-- Copyright:  (c) Ertugrul Soeylemez, 2013
--                 Rongcui Dong, 2015
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Signal
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

data Signal s m a b where
    SId :: Signal s m a a
    SConst :: Maybe b -> Signal s m a b
    SArr :: (Maybe a -> Maybe b) -> Signal s m a b
    SPure :: (s -> Maybe a -> (Maybe b, Signal s m a b)) -> Signal s m a b
    SGen :: (Monad m) => 
            (s -> Maybe a -> m (Maybe b, Signal s m a b)) -> Signal s m a b


instance (Monad m) => Category (Signal s m) where
    id = SId
    s2 . s1 = SGen $ \ds mx0 -> do
                (mx1, s1') <- stepSignal s1 ds mx0
                (mx2, s2') <- stepSignal s2 ds mx1
                mx2 `seq` return (mx2, s2'. s1')
    
instance (Monad m) => Arrow (Signal s m) where
    arr f = SArr (fmap f)
    first s = SGen f
        where
          f ds mxy = 
              let mx = fst <$> mxy
                  my = snd <$> mxy
                  mmxs' = stepSignal s ds mx in
                liftM (g my) mmxs'
          g my (mx', s') = 
              let mx'y = (,) <$> mx' <*> my in
              lstrict (mx'y, first s')

instance (Monad m) => ArrowChoice (Signal s m) where
  left s =
    SGen $ \ds mmx ->
    liftM (fmap Left ***! left) . stepSignal s ds $
    case mmx of
      Just (Left x) -> Just x
      Just (Right x) -> Nothing
      Nothing -> Nothing
      
  right s =
    SGen $ \ds mmx ->
    liftM (fmap Right ***! right) . stepSignal s ds $
    case mmx of
      Just (Left x) -> Nothing
      Just (Right x) -> Just x
      Nothing -> Nothing

  sl +++ sr =
    SGen $ \ds mmx ->
    case mmx of
    Just (Left x) -> do
      liftM2 (\ (mx,sl')(_,sr') -> lstrict (fmap Left mx, sl' +++ sr'))
        (stepSignal sl ds (Just x)) (stepSignal sr ds Nothing)
    Just (Right x) -> do
      liftM2 (\ (_,sl')(mx,sr') -> lstrict (fmap Right mx, sl' +++ sr'))
        (stepSignal sl ds Nothing) (stepSignal sr ds (Just x))
    Nothing ->
      liftM2 (\ (_,sl')(_,sr') -> lstrict (Nothing, sl' +++ sr'))
        (stepSignal sl ds Nothing) (stepSignal sr ds Nothing)

  sl ||| sr =
    SGen $ \ds mmx ->
    case mmx of
    Just (Left x) -> do
      liftM2 (\(mx,sl')(_,sr') -> lstrict (mx, sl' ||| sr'))
        (stepSignal sl ds (Just x)) (stepSignal sr ds Nothing)
    Just (Right x) -> do
      liftM2 (\(_,sl')(mx,sr') -> lstrict (mx, sl' ||| sr'))
        (stepSignal sl ds Nothing) (stepSignal sr ds (Just x))
    Nothing -> do
      liftM2 (\(_,sl')(_,sr') -> lstrict (Nothing, sl' ||| sr'))
        (stepSignal sl ds Nothing) (stepSignal sr ds Nothing)

instance (MonadFix m) => ArrowLoop (Signal s m) where
  loop s =
    SGen $ \ds mx ->
      liftM (fmap fst ***! loop) .
      mfix $ \ ~(mx',_) ->
        let d | Just (_,d) <- mx' = d
              | otherwise = error "Feedback broken by inhibition"
        in stepSignal s ds (fmap (,d) mx)

instance (Monad m) => Functor (Signal s m a) where
    fmap f SId = SArr $ fmap f
    fmap f (SConst mx) = SConst $ fmap f mx
    fmap f (SArr g) = SArr $ fmap f . g
    fmap f (SPure g) = SPure $ \ds -> (fmap f ***! fmap f) . g ds

instance (Monad m) => Applicative (Signal s m a) where
    pure = SConst . Just
    sf <*> sx = 
        SGen $ \ds mx ->
        liftM2 (\(mf, sf) (mx, sx) -> lstrict (mf <*> mx, sf <*> sx))
        (stepSignal sf ds mx)
        (stepSignal sx ds mx)

            

-- | Steps a signal in certain time step
stepSignal :: (Monad m) =>
              Signal s m a b 
           -- ^ Signal to be stepped
           -> s
           -- ^ Delta session
           -> Maybe a 
           -- ^ Input
           -- | Stateful output
           -> m (Maybe b, Signal s m a b)
stepSignal s@(SId) _ mx = return (mx, s)
stepSignal s@(SConst mx) _ _ = return (mx, s)
stepSignal s@(SArr f) _ mx = return (f mx, s)
stepSignal s@(SPure f) ds mx = return (f ds mx)
stepSignal s@(SGen f) ds mx = f ds mx

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

