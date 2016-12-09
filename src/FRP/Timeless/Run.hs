-- |
-- Module:     FRP.Timeless.Run
-- Copyright:  (c) 2013 Ertugrul Soeylemez (c) 2015 Rongcui Dong
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
import FRP.Timeless.Internal.Signal
import Data.Functor.Identity
import System.IO

-- | This function runs the given signal network using the given state delta
-- generator.  It constantly shows the output of the wire on one line
-- on stdout.  Press Ctrl-C to abort.

testSignal ::
    (MonadIO m, Show b)
    => (forall a. Signal Identity a b)
    -> m c
testSignal n0 = loop n0
    where
    loop n = do
        let Identity (mx, n') = stepSignal n (Just ())
        liftIO $ do
            putChar '\r'
            putStr (maybe "Inhibited" show mx)
            putStr "\027[K"
            hFlush stdout
        loop n'

-- | This command drives a black box of signal network. The driver
-- knows nothing about the internals of the network, only stops when
-- the network is inhibited.
runBox :: (Monad m) => Signal m () () -> m ()
runBox n = do
  (mq, n') <- stepSignal n (Just ())
  case mq of
    Just _ -> n' `seq` runBox n'
    Nothing -> return ()
