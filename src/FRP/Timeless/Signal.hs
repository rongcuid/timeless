module FRP.Timeless.Signal
    where

import Prelude hiding ((.),id)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Monoid 
import Control.Category

data Signal t m a b where
    SId :: Signal t m a a
    SConst :: Maybe b -> Signal t m a b
    SArr :: (Maybe a -> Maybe b) -> Signal t m a b
    SPure :: (t -> Maybe a -> (Maybe b, Signal t m a b)) -> Signal t m a b
    SGen :: (Monad m) => 
            (t -> Maybe a -> m (Maybe b, Signal t m a b)) -> Signal t m a b

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

-- | The Identity Signal
mkId :: (Monad m) => Signal s m a a 
mkId = SId

-- | Make a constant Signal
mkConst :: (Monad m) => Maybe b -> Signal t m a b
mkConst = SConst 

-- | Make a signal that inhibits forever
mkEmpty :: (Monad m) => Signal t m a b
mkEmpty = SConst Nothing

-- | Make a stateful signal from given (Monadic) transition function
mkGen :: (Monad m, Monoid t) => (t -> a -> m (Maybe b, Signal t m a b)) -> Signal t m a b
mkGen f = go mempty
    where
      go t0 = SGen $ \dt mx ->
          let t = t0 <> dt in 
          t `seq` 
            case mx of
              Just x -> liftM lstrict (f t x)
              Nothing -> return (Nothing, go t)

-- | Make a stateful signal from given (Monadic) time independant transition function
mkGenN :: (Monad m) => (a -> m (Maybe b, Signal t m a b)) -> Signal t m a b
mkGenN f = go 
    where
      go = SGen $ \_ mx ->
           case mx of
             Just x -> liftM lstrict (f x)
             Nothing -> return (Nothing, go)

-- | Make a stateless signal from given function
mkGen_ :: (Monad m) => (a -> m (Maybe b)) -> Signal t m a b
mkGen_ f = go 
    where
      go = SGen $ \_ mx ->
           case mx of
             Just x -> 
                 let mmx' = f x in
                 liftM (lstrict . (, go)) mmx'
                 -- ^ From (m (Maybe b)) to (m (Maybe b, Signal t m a b))
             Nothing ->
                 return (Nothing, go)

-- | Make a pure stateful signal from given transition function
mkPure :: (Monoid t) => (t -> a -> (Maybe b, Signal t m a b)) -> Signal t m a b
mkPure f = go mempty
    where
      go t0 = SPure $ \dt mx ->
          let t = t0 <> dt in 
          t `seq` 
            case mx of
              Just x -> lstrict (f t x)
              Nothing -> (Nothing, go t)

-- | Make a pure stateful signal from given time independant transition function
mkPureN :: (a -> (Maybe b, Signal t m a b)) -> Signal t m a b
mkPureN f = go 
    where
      go = SPure $ \_ mx ->
           case mx of
             Just x -> lstrict (f x)
             Nothing -> (Nothing, go)

-- | Make a pure stateless signal from given function
mkPure_ :: (a -> (Maybe b)) -> Signal t m a b
mkPure_ f = go 
    where
      go = SPure $ \_ mx ->
           case mx of
             Just x -> lstrict (f x, go)
                 -- ^ From (m (Maybe b)) to (m (Maybe b, Signal t m a b))
             Nothing -> (Nothing, go)

-- | Make a pure stateful wire from given transition function
mkPW :: (Monoid t) => (t -> a -> (b, Signal t m a b)) -> Signal t m a b
mkPW f = mkPure (\dt -> lstrict . first (Just) . (f dt)) 
-- first (Just) is (a, b) -> (Maybe a, b)

-- | Make a pure stateful wire from given time independant transition function
mkPWN :: (a -> (b, Signal t m a b)) -> Signal t m a b
mkPWN f = mkPureN $ lstrict . first (Just) . f

-- | Make a pure stateless wire from given function
mkPW_ :: (a -> b) -> Signal t m a b
mkPW_ = SArr . fmap


-- | Steps a signal in certain time step
stepSignal :: (Monad m) => Signal t m a b 
           -- ^ Signal to be stepped
           -> t
           -- ^ Delta time
           -> Maybe a 
           -- ^ Input
           -- | Stateful Kleisli output
           -> m (Maybe b, Signal t m a b)
stepSignal s@(SId) _ mx = return (mx, s)
stepSignal s@(SConst mx) _ _ = return (mx, s)
stepSignal s@(SArr f) _ mx = return (f mx, s)
stepSignal s@(SPure f) dt mx = return (f dt mx)
stepSignal s@(SGen f) dt mx = f dt mx

lstrict :: (a,b) -> (a,b)
lstrict (x,y) = x `seq` (x,y)
