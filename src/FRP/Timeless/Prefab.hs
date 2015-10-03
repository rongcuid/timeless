-- |
-- Module:     FRP.Timeless.Prefab
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Prefab 
    (-- * Factory
     mkPW
    , mkPWN
    , mkPW_
    , mkSW_
    , mkKleisli_
    )
    where

-- | Make a pure stateful wire from given transition function
mkPW :: (Monoid s) => (s -> a -> (b, Signal s m a b)) -> Signal s m a b
mkPW f = mkPure (\ds -> lstrict . first (Just) . (f ds)) 
-- first (Just) has type (a, b) -> (Maybe a, b)

-- | Make a pure stateful wire from given time independant transition function
mkPWN :: (a -> (b, Signal s m a b)) -> Signal s m a b
mkPWN f = mkPureN $ lstrict . first (Just) . f

-- | Make a pure stateless wire from given function
mkPW_ :: (a -> b) -> Signal s m a b
mkPW_ = SArr . fmap

-- | Make a stateful wire from chained state transition function
mkSW_ :: b -> (b -> a -> b) -> Signal s m a b
mkSW_ b0 f = mkPWN $ g b0
    where
      g b0 x = let b1 = f b0 x in
               (b1, mkSW_ b1 f)

-- | Make a stateless signal from Kleisli function
mkKleisli_ :: (Monad m) => (a -> m b) -> Signal s m a b
mkKleisli_ f =  mkGen_ $ \x -> fmap Just (f x)

