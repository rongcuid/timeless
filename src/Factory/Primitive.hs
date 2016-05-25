module Factory.Primitive where

import           Control.Arrow
import           Signal

mkEmpty = SEmpty
mkId = SId
mkConst = SPure . const
mkPure = SPure

-- | A monadic action
mkAction :: Monad m => (a -> m b) -> Signal m a b
mkAction = SGen () . runKleisli . first . Kleisli

-- | A control block
mkCtrl :: Monad m => (Maybe a -> m (Maybe b)) -> Signal m a b
mkCtrl = SCtrl

-- | A generator signal
mkGen :: Monad m => s -> ((a, s) -> m (b, s)) -> Signal m a b
mkGen = SGen
