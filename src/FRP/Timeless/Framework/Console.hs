-- |
-- Module:     FRP.Timeless.Framework.Console
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.Console
       where

import Prelude hiding ((.), id)
import FRP.Timeless
import System.IO
import System.Console.ANSI
import Control.Monad
import Linear
import Control.Monad.IO.Class

data InitConfig = InitConfig
    {
      initInHandle :: Handle
    , initOutHandle :: Handle
    , initEcho :: Bool
    , initInBuffering :: BufferMode
    , initOutBuffering :: BufferMode
    , initShowCursor :: Bool
    }

defaultInitConfig = InitConfig stdin stdout False NoBuffering NoBuffering False

-- | Initializes console
initConsole :: InitConfig -> IO ()
initConsole conf = do
  let hIn = initInHandle conf
      hOut = initOutHandle conf
  clearScreen
  hSetEcho hIn False
  hSetBuffering hIn $ initInBuffering conf
  hSetBuffering hOut $ initOutBuffering conf
  if initShowCursor conf then
      hShowCursor hOut else
      hHideCursor hOut


-- | Draw a filled ascii box with specified color and size. It will
-- destroy SGR color state, be careful
asciiBox :: Int -> Int -> ColorIntensity -> Color -> IO ()
asciiBox w h i c = do
  let tbLine = "+" ++ (replicate (w-2) '-') ++ "+"
  setCursorPosition 0 0
  setSGR [SetColor Foreground i c]
  putStrLn tbLine
  mapM_ (\rol -> drawChar '|' rol 0 i c >> drawChar '|' rol (w-1) i c) [1..h-2]
  setSGR [SetColor Foreground i c]
  setCursorPosition (h-1) 0
  putStrLn tbLine
  setSGR [Reset]

-- | Draw a character at a specific position. It will destroy SGR
-- color state
drawChar :: Char -> Int -> Int -> ColorIntensity -> Color -> IO ()
drawChar c rol col i color = do
  setCursorPosition rol col
  setSGR [SetColor Foreground i color]
  putChar c
  setSGR [Reset]

-- | Statefully draw character
drawCharS :: Char -- ^ The character
           -> ColorIntensity -> Color
           -> V2 Int -- ^ Previous Position, XY coordinate (Not R-C)
           -> V2 Int -- ^ Next Position, XY coordinate
           -> IO (V2 Int)
drawCharS c i col p@(V2 c0 r0) p'@(V2 c' r')
    | p /= p' =
        do
          drawChar ' ' r0 c0 i col
          drawChar c r' c' i col
          return p'
    | otherwise = return p

-- | Moves a character
sMoveChar :: MonadIO m =>
             Char
          -> ColorIntensity -> Color
          -> V2 Int
          -> Signal m (V2 Int) ()
sMoveChar char int col p0@(V2 r c) = mkSK_ p0 f >>> mkConstM (return ())
  where
    f p p' = liftIO $ drawCharS char int col p p'

-- | Clears a certain column range on a certain row
clearLineRange :: Int -- ^ Row
               -> Int -- ^ Beginning Col
               -> Int -- ^ End Col, non inclusive
               -> IO ()
clearLineRange r c c' = do
  setCursorPosition r c
  let l = c' - c
      cover = replicate l ' '
  putStr cover

-- | Gets character input from console without blocking
sInputNonBlocking :: Signal s IO () (Maybe Char)
sInputNonBlocking = mkActM f
  where
    f :: IO (Maybe Char)
    f = do
      b <- hReady stdin
      case b of
        True -> Just <$> getChar
        False -> return Nothing

