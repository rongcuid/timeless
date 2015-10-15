-- | 
-- Module:     FRP.Timeless.Tutorial2
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

{-# LANGUAGE Arrows #-}

module FRP.Timeless.Tutorial2
       (
       -- * Introduction
       -- $Introduction

       -- * Input/Output
       -- $IO
       sInput
       , drawPlayer
       , testIO
         
       -- * Game State
       -- ** Player State
       -- $GameState-Player
       , Move(..)
       , updatePosX
       , sPlayerX
       , toMove
       , testPlayer
         
       -- ** Enemy State
       -- $GameState-Enemy
       , EnemyEvent(..)
       , Enemy(..)
       , sUpdateEnemy
       , dPos
       )
       where

import System.Console.ANSI
import FRP.Timeless
import FRP.Timeless.Framework.Console
import Data.Char




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
-- 58x28, and changes color on request. We won't make that a separate
-- signal since semantically, we cannot guarentee the evaluation order
-- of different 'Signal's. We will need a unified 'sOutput', which
-- will be built along this tutorial.
--
-- We will implement a function to draw the player spaceship, which is
-- just a charcter @^@ on the bottom line. Making use of the
-- 'drawChar' function provided, we can easily create a signal
-- 'drawPlayer' (read the source), which takes the column number and
-- draws the player. However, if we just implement by clearing a line
-- and drawing a new character, serious flicker will
-- appear. Therefore, we implement a stateful version that keeps track
-- of the previous position. This function can easily be composed into
-- a larger stateful render action. Notice that the initial value of
-- 'drawChar' is set to (-1) since it is an impossible value for a
-- normal update, so the console is rendered correctly on the first
-- "frame". A similar function provided in the framework is
-- 'drawCharS'
--
-- Again, testing with @timeless@ is easy: Just connect a simple
-- box. Here, the test function is `testIO`, where almost everything
-- is hard coded. But for a testing, it is fine. Be careful that if
-- you interrupt the program, it is likely to color stain your console.



sInput = sInputNonBlocking

-- | Draws a vivid white @^@ on the bottom line, and deals with flickering
drawPlayer :: Int -> Int -> IO Int
drawPlayer c c'
    | c /= c' =
      do
        -- v Clear line
        clearLineRange 28 1 59
        -- V Draw character
        drawChar '^' 28 c' Vivid White
        return c'
    | otherwise = return c

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
        x <- (arr fBound) <<< (mkSW_ 30 fMove) -< mc
        mkSK_ (-1) drawPlayer -< x
        returnA -< ()

-- $GameState-Player
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
-- `Move' is just an enumerate.
-- 
-- > Int -> Move -> Int
--
-- Look at 'sPlayerX' to get an idea. In order to get the 'Move' data
-- from user input, we need another signal that converts `Maybe Char`
-- to 'Move':
--
-- > Signal s m (Maybe Char) Move
--
-- We will call this function 'toMove'. Again, try to construct a box
-- for testing: `testPlayer`. 

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
toMove Nothing = MStay
toMove (Just c)
    | toLower c == 'a' = MLeft
    | toLower c == 'd' = MRight
    | otherwise = MStay

testPlayer = initConsole defaultInitConfig >> runBox clockSession_ b
    where
      b = proc _ -> do
        mc <- sInput -< ()
        mv <- arr toMove -< mc
        x <- sPlayerX -< mv
        mkSK_ (-1) drawPlayer -< x
        returnA -< ()

-- $GameState-Enemy
--
-- After getting a working player state, we are going to create the
-- data and states for ememies. For this simple demo game, the enemy
-- only keeps three states: Position, movement, and live. Let's make a
-- data type for enemy, 'Enemy', and a data type for enemy related
-- events, 'EnemyEvent'. Since we want modularity, we will compose a
-- big signal, 'sUpdateEnemy' with the type:
--
-- > sUpdateEnemy :: (Monad m) => Enemy -> Signal s m EnemyEvent Enemy
--
-- Inspecting the type, we know that this signal is pure. We are going
-- to create this signal from some small and reusable ones. Check the
-- source to have a glance. I will explain each part.
--
-- First, let's handle the position update. "Physically", the position
-- is just the integral of velocity over time. Note that although
-- enemies can only occupy integer positions, internally we can keep
-- fractional positions. Therefore, we use the following prefab signal:
--
-- > integrateWith :: (Monad m, Monoid b, Monoid s) => b -> (s -> a -> b) -> Signal s m a b
--
-- Does it look familiar? It looks just like an extension of the
-- `mkSW_` factory! Let's first guess what this signal does from the
-- type of its factory.
--
-- `s` is a 'Monoid' because it is the delta 'Session', or time. The
-- input type is 'a', output type is `b`, and it has to be a
-- 'Monoid'. We are supplying one single `b`, and a function with type
-- `s -> a -> b`. First, the single `b` is probably an initial
-- state. Then, being a 'Monoid' means that two `b`s can be combined
-- together. If the function supplied just generates a new `b` like
-- `mkSW_` does, then there is no need to make it a
-- `Monoid`. Therefore, we can conclude that the function supplied
-- must produce the difference between the current state and the next
-- state.
--
-- Here, our type of position is '(Double, Double)', so is that of
-- speed. Therefore, we get a difference function 'dPos'.
--
-- Then, we need the velocity vector to get this function work. We
-- simply multiply the speed with the direction vector. Notice that we
-- have a tuple as our direction vector, so `fmap` won't work
-- here. Instead, we need to make use of Arrow combinators:
--
-- > ((*speed) *** (*speed)) direction
--
-- Here, the Arrow is (->) but not 'Signal s m'. Of course, this is
-- not a pretty solution. In actual games, it is better to use 'V2'
-- from "Linear" package, but I don't want to introduce an additional
-- dependancy just for a tutorial.
--

data EnemyEvent = EKill

data Enemy = Enemy {
      ePos :: (Int, Int) -- ^ Enemy Position
    , eDir :: (Int, Int) -- ^ Direction vector
    , eSpeed :: Double -- ^ Speed, affecting update interval
    , eAlive :: Bool -- ^ Is it alive?
    }

-- | Modeling the change of position
dPos :: (HasTime t s) => s -- ^ Delta session/time
     -> (Double, Double)
     -- ^ Velocity vector
     -> (Double, Double)
        -- ^ delta Position
dPos s v = let t = realToFrac $ dtime s
               f = (*t) *** (*t)
           in f v

sUpdateEnemy :: (Monad m) => Enemy -> Signal s m EnemyEvent Enemy
sUpdateEnemy e0 =
  let (x0,y0) = ePos e0
      (x0f, y0f) = (fromIntegral x0, fromIntegral y0)
      (dX0, dY0) = eDir e0
      s0 = eSpeed e0
      a0 = eAlive e0
  in proc ev -> do
    
      returnA -< e0 -- Placeholder
