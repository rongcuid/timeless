-- | Tutorial 2 for 'timeless'.
-- Module:     FRP.Timeless.Tutorial2
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

{-# LANGUAGE Arrows #-}

module FRP.Timeless.Tutorial2
       where

import System.Console.ANSI
import FRP.Timeless
import FRP.Timeless.Framework.Console
import Data.Char

-- * Introduction
-- $Introduction
-- * Input/Output
-- $IO
-- * Game State
-- $GameState


-- $Introduction
--
-- In this tutorial, we are going to use the framework
-- "FRP.Timeless.Framework.Console" to build a small console game, "SpHase
-- Invadors"!
--
-- The console framework is based on "System.Console.ANSI", so make
-- sure you have that installed.
--
-- Also, as a prerequisite, please read tutorial 1 in module
-- "FRP.Timeless.Tutorial". Sorry that I did not expect Haddock doesn't work as
-- I expected with Literate Haskell, so please read the source of that instead.

-- $IO
--
-- First, since this is a game, we cannot block IO. Therefore, we use
-- 'sInputNonBlocking'. Notice that the signals in this tutorial may have
-- different meanings from the previous one. In this case, 'sInputNonBlocking'
-- no longer waits for an interval, but just checks whether input is available.
--
-- > sInput = sInputNonBlocking
--
-- To make a nice output for the game, let's consider creating a
-- bounding box that is 60x30 character size, giving a board size of
-- 58x28, and changes color on request. Let's make such output signal:
--
-- > sBoundingBox = sAsciiBox 60 30
--
-- Then, we are preparing to draw the player spaceship, which is just
-- a charcter @^@ on the bottom line. Making use of the 'drawChar'
-- function provided, we can easily create a signal 'drawPlayer' (read
-- the source), which takes the column number and draws the player.
--
-- Again, testing with @timeless@ is easy: Just connect a simple
-- box. Here, the test function is `testIO`, where almost everything
-- is hard coded. But for a testing, it is fine. Be careful that if
-- you interrupt the program, it is likely to color your console.



sInput = sInputNonBlocking
sBoundingBox = sAsciiFillColorBox 60 30

-- | Draws a vivid white @^@ on the bottom line
drawPlayer :: Int -- ^ The row number of bottom line
           -> Signal s IO Int ()
drawPlayer r = mkKleisli_ $ \c -> drawChar '^' r c Vivid White

testIO = initConsole defaultInitConfig >> runBox clockSession_ b
    where
      fMove x0 (Just c) | toLower c == 'a' = x0-1
                        | toLower c == 'd' = x0+1
                        | otherwise = x0
      fMove x0 _ = x0
      fBound x | x > 59 = 59
               | x < 1 = 1
               | otherwise = x
      b = proc _ -> do
        mc <- sInput -< ()
        sBoundingBox -< (Vivid, Green)
        x <- (arr fBound) <<< (mkSW_ 40 fMove) -< mc
        drawPlayer 29 -< x
        returnA -< ()

-- $GameState
-- 
-- Next, we are going to make something complicated. Considering that
-- this is a _game_, we don't want the objects to move
-- unevenly. At the same time, we want to give the ability to 'cooldown'.
--
-- Before all that, let's look at the central game states. Player first!
--
-- Since only the X coordinate matters, our state for the player is
-- just one integer, giving an overall state transition function of
-- type, where 'Move' is the type for the player input. In this game,
-- `Move` is just an enumerate.
-- 
-- > Int -> Move -> Int
--
-- Look at 'sPlayerX' to get an idea. In order to get the 'Move' data
-- from user input, we need another signal that converts 'Maybe Char'
-- to 'Move':
--
-- > Signal s m (Maybe Char) Move
--
-- We will call this function 'toMove'.

data Move = MLeft | MRight | MStay


-- | Updates an X coordinate
updatePosX x MLeft | x <= 1 = 1
                   | otherwise = x-1
updatePosX x MRight | x >= 59 = 59
                    | otherwise = x+1
updatePosX x _ = x

-- | The stateful signal of player X coordinate
sPlayerX :: (Monad m) => Signal s m Move Int
sPlayerX = mkSW_ 30 updatePosX -- 30 is the middle

-- | Convert a keypress to 'Move'
toMove :: (Monad m) => Signal s m (Maybe Char) Move
toMove = arr f
    where
      f Nothing = MStay
      f (Just c)
        | toLower c == 'a' = MLeft
        | toLower c == 'd' = MRight
        | otherwise = MStay
