-- |
-- Module:     Control.Wire.Run
-- Copyright:  Original work (c) 2013 Ertugrul Soeylemez
-- Copyright:  Derived work (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Run

    ( -- * Testing signal network
      testSignal
      -- * Running a network
    , runBox
    )
    where

import Control.Monad.IO.Class
import FRP.Timeless.Session
import FRP.Timeless.Signal
import Data.Functor.Identity
import System.IO

-- | This function runs the given signal network using the given state delta
-- generator.  It constantly shows the output of the wire on one line
-- on stdout.  Press Ctrl-C to abort.

testSignal ::
    (MonadIO m, Show b)
    => Session m s
    -> (forall a. Signal s Identity a b)
    -> m c
testSignal s0 n0 = loop s0 n0
    where
    loop s n = do
        (ds, s') <- stepSession s
        let Identity (mx, n') = stepSignal n ds (Just ())
        liftIO $ do
            putChar '\r'
            putStr (maybe "Inhibited" show mx)
            putStr "\027[K"
            hFlush stdout
        loop s' n'

-- | This command drives a black box of signal network. The driver
-- knows nothing about the internals of the network, only stops when
-- the network is inhibited.
runBox :: (Monad m) => Session m s -> Signal s m () () -> m ()
runBox s n = do
  (ds, s') <- stepSession s
  (mq, n') <- stepSignal n ds (Just ())
  case mq of
    Just _ -> runBox s' n'
    Nothing -> return ()
