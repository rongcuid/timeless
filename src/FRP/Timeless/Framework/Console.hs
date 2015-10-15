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
           -> (Int, Int) -- Previous Position
           -> (Int, Int) -- Next Position
           -> IO (Int, Int)
drawCharS c i col (r0,c0) (r',c')
    | (r0,c0) /= (r',c') =
        do
          drawChar ' ' r0 c0 i col
          drawChar c r' c' i col
          return (r', c')
    | otherwise = return (r0, c0)

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

