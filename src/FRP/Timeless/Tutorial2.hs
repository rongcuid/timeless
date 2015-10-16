-- | 
-- Module:     FRP.Timeless.Tutorial2
-- Copyright:  (c) Rongcui Dong, 2015
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

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
       , dPos
       , sUpdateEnemy0
       , testEnemy

       -- ** Player State, Again
       -- $GameState-Player2
       , Player(..)
       , PlayerEvent(..)
       , updatePosX'
       , sToFire
       , sFire
       , sUpdatePlayer
       , toPlayerEvent
       , testPlayer2
         
       -- ** Firing Bullets
       -- $GameState-Bullets
       , Bullet(..)
       , BulletEvent(..)
       , sUpdateBullet

       -- ** Enemy State, Again
       -- $GameState-Enemy2
       )
       where

import System.Console.ANSI
import FRP.Timeless
import FRP.Timeless.Framework.Console
import Data.Char
import Data.Monoid
import Linear
import Linear.Affine
import qualified Debug.Trace as D

-- $Introduction
--
-- In this tutorial, we are going to use the framework
-- "FRP.Timeless.Framework.Console" to build a small console game,
-- "SpHase Invadoors"!
--
-- The console framework is based on "System.Console.ANSI", so make
-- sure you have that installed.
--
-- Also, as a prerequisite, please read tutorial 1 in module
-- "FRP.Timeless.Tutorial". Sorry that I did not expect Haddock
-- doesn't work as I expected with Literate Haskell, so please read
-- the source of that instead.
--
-- Just a warning, this tutorial is written as a guide for my
-- development. Therefore, it is the case that @timeless@ will be much
-- more extensive at the end of the tutorial than it is at the
-- beginning. Also, my understanding of the problem will change as I
-- write this tutorial, so expect some style difference in the code,
-- and probably quite a few wrappers to adapt older code to newer
-- ones. Nevertheless, these wrappers and adapters should show
-- multiple ways how a @timeless@ program can be structured.


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

data Move = MLeft | MRight | MStay deriving (Show)


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
        mkActM $ asciiBox 60 30 Vivid Green -< ()
        mc <- sInput -< ()
        mv <- arr toMove -< mc
        x <- sPlayerX -< mv
        mkSK_ (-1) drawPlayer -< x
        returnA -< ()

-- $GameState-Enemy
--
-- As a foreword, run 'testEnemy' to see how enemy data is
-- updated. Press 'k' (lower case, for simplicity) to kill the enemy
--
-- After getting a working player state, we are going to create the
-- data and states for ememies. For this simple demo game, the enemy
-- only keeps three states: Position, movement, and live. Let's make a
-- data type for enemy, 'Enemy', and a data type for enemy related
-- events, 'EnemyEvent'. Since we want modularity, we will compose a
-- big signal, 'sUpdateEnemy0' with the type:
--
-- > sUpdateEnemy0 :: (Monad m) => Enemy -> Signal s m EnemyEvent Enemy
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
-- > integrateM :: (Monad m, Monoid b, Monoid s) => b -> (s -> a -> b) -> Signal s m a b
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
-- As stated, we need a 'Monoid' for integration. Since numerical
-- integration involves summing the results of each step, if we use
-- the 'integrateM' factory, we will use the monoid 'Sum' provided by
-- "Data.Monoid". However, since numerical integration is so common,
-- there is a more convenient signal factory:
--
-- > integrate :: (Monad m, Num a, Monoid s) => a -> (s -> a -> a) -> Signal s m a a
--
-- In the code of 'sUpdateEnemy0', this line models the system of enemy
-- position:
--
-- > p' <- integrate p0 dPos -< dP'
--
-- It reads very clear: integratethe velocity vector (__dP__) over
-- time using function 'dPos' to get the enemy position.
--
-- Then, since our 'Enemy' type stores integer position, we need to
-- round the fractional position:
--
-- > iP' <- arr (fmap round) -< p'
--
-- The other thing to model is whether the enemy is alive. This is
-- easy too:
--
-- > a' <- latchR <<< arr (\case {EKill -> True; _ -> False}) -< ev
--
-- I used the LambdaCase extension to make the lambda very
-- short. 'latchR' is one of the latches provided in
-- "FRP.Timeless.Prefab", which outputs 'True' until the input becomes
-- 'True', then it latches at 'False'. If you know digital circuits,
-- think "Reset Latch". Side note: as you may guess, there are
-- actually three latches at the time of writing: 'latch', 'latchS',
-- 'latchR'.
--
-- A latch is used here because events are discrete, and we know that
-- enemies die when they are killed. Of course the hero don't like to
-- see that an enemy has to be constantly killed to be dead!
--
-- Finally, we just return an updated copy of enemy, and return. Don't
-- worry about constantly making new objects: the garbage collector
-- will do its work. Also note that due to Haskell's laziness, if a
-- Signal is not used, it will not be evaluated. If you try to debug a
-- signal, make sure to wire it up to 'sDebug' somewhere, or use its
-- output in some way that forces evaluation (such as printing or
-- pattern matching).
--
-- The 'testEnemy' function is easy, too. We just takes input, create
-- events out of them, feed the event to an enemy, and finally print
-- the enemy. The print signal here is written in a way so that
-- information will not flood the console.



data EnemyEvent = EKill | ENoevent

data Enemy = Enemy {
      ePos :: Point V2 Int -- ^ Enemy Position
    , eDir :: V2 Int -- ^ Direction vector
    , eSpeed :: Double -- ^ Speed, affecting update interval
    , eAlive :: Bool -- ^ Is it alive?
    }

instance Show Enemy where
  show e = "[Enemy] At: "
           ++ (show (x,y))
           ++ " Velocity: "
           ++ (show (vx,vy))
           ++ " Alive: "
           ++ show (eAlive e)
               where
                 P (V2 x y) = ePos e
                 V2 vx vy = (fromIntegral <$> eDir e) / (pure $ eSpeed e)

-- | Modeling the change of position
dPos :: (HasTime t s) =>
       s
    -- ^ Delta session/time
    -> V2 Double
    -- ^ Velocity vector
    -> V2 Double
        -- ^ delta Position
dPos s v = let dt = realToFrac $ dtime s
           in v * dt

-- | Main signal to update an enemy
sUpdateEnemy0 :: (Monad m, HasTime t s) =>
                Enemy
                -> Signal s m EnemyEvent Enemy
sUpdateEnemy0 e0 =
  let P p0 = fromIntegral <$> ePos e0
      -- ^ initial position
      uP0 = eDir e0
      -- ^ Direction vector
      v = eSpeed e0
      -- ^ Speed value
      -- | Is it alive?
      a0 = eAlive e0
  in proc ev -> do
    uP' <- mkId -< uP0 -- Direction does not change for now
    dP' <- arr (\u -> (fromIntegral <$> u) / (pure v)) -< uP'
    -- v Integrate speed to get position
    p' <- integrate p0 dPos -< dP'
    -- v Round position to get the row/col position
    iP' <- arr (fmap round) -< p'
    a' <- latchR <<< arr (\case {EKill -> True; _ -> False}) -< ev
    -- v Return the updated enemy
    let e' = e0 {ePos = P iP', eAlive = a'}
    returnA -< e'

testEnemy :: IO ()
testEnemy = runBox clockSession_ b
    where
      enemy = Enemy (P $ V2 1 1) (V2 1 1) 1 True
      b = proc _ -> do
        mc <- sInput -< ()
        ev <- arr (\case {Just 'k' -> EKill; _ -> ENoevent}) -< mc
        e' <- sUpdateEnemy0 enemy -< ev
        mkKleisli_ putStr -< show e' ++ "\r"
        returnA -< ()

-- $GameState-Player2
--
-- Now, learning from our enemies, we can also package our player
-- up. Our player does not die, but it does move, and fire
-- bullets. Therefore, we can make the 'Player' and 'PlayerEvent' data
-- types. Similarly, we will make an 'sUpdatePlayer' signal.
--
-- To get a feeling of how it works, run 'testPlayer2'. Press 'a' or
-- 'd' to move around, and '<Space>' to fire. Note that you won't be
-- able to see it fire unless your computer is unacceptably slow:
-- Firing is an impulse which is supposed to take infinitely short
-- time. Though it is technically impossible to achieve this, a
-- reasonable computer should process this fast enough to make the
-- change impossible to see.
--
-- Modeling the position is trivil here too:
--
-- > x' <- mkSW_ x0 updatePosX' -< ev
--
-- We are almost doing the same thing as 'sPlayerX'. The only
-- differences are that we no longer hard code the initial position
-- here, and we use 'PlayerEvent' instead of 'Move'.
--
-- Now, it comes to firing bullets. Different from moving, which can
-- be done as fast as the player hits the keyboard, the cannon of
-- player's ship needs cooldown. To achieve this, we are going to use
-- the switching ability of signals.
--
-- > (-->) :: (Monad m) => Signal s m a b -> Signal s m a b -> Signal s m a b
--
-- A signal, @s1 --> s2@, will produce the output of @s1@ so long as
-- it is active. When @s1@ inhibits, the signal then activates and
-- switches to @s2@, and never come back. The timing of cannon can be
-- done surprisingly easy:
--
-- > sFire = (Nothing <=> Just False) --> oneShot True --> (pure False >>> wait 0.25) --> sFire
--
-- '(<=>)' is a convenient signal that takes 'Bool' as input, and
-- outputs the left value when 'True' and right value when
-- 'False'. Remember that 'Nothing' denotes inhibition. 'oneShot'
-- resembles an impulse function, which produces an output for a
-- semantically infinitely short periods of time. The signal reads
-- very straightforward: When input is 'False', output 'False'; once
-- input becomes 'True'(Fire key is pressed), output 'True' for one
-- shot, then stay 'False' regardless of input for 0.25 seconds, then
-- reset.

data Player = Player {
      playerPos :: Point V2 Int -- ^ Position
    , playerFiring :: Bool -- ^ Whether player is firing bullets
    }

instance Show Player where
  show p = "[Player] Position: "
           ++ (show (x, y))
           ++ " Firing: "
           ++ (show $ playerFiring p)
      where
        P (V2 x y) = playerPos p

data PlayerEvent = PMoveL | PMoveR | PFire | PNoevent deriving (Show)

-- | Just a wrapper around 'updatePosX'
updatePosX' x PMoveL = updatePosX x MLeft
updatePosX' x PMoveR = updatePosX x MRight
updatePosX' x _ = updatePosX x MStay

-- | Interface 'PlayerEvent' to logic signal
sToFire :: Monad m => Signal s m PlayerEvent Bool
sToFire = arr f
    where
      f PFire = True
      f _ = False

-- | The logic signal to handle fire and cooldown
sFire :: (HasTime t s, Monad m) => Signal s m Bool Bool
sFire = (Nothing <=> Just False)
        --> oneShot True
        --> waitWith False 0.25
        --> sFire

-- | The signal to update player
sUpdatePlayer :: (Monad m, HasTime t s) =>
                 Player
                 -> Signal s m PlayerEvent Player
sUpdatePlayer pl0 =
  let P p0@(V2 x0 y0) = playerPos pl0
      -- ^ Initial position
  in proc ev -> do
    -- v Get X coordinate from event
    x' <- mkSW_ x0 updatePosX' -< ev
    -- v Fire bullets
    firing <- sFire <<< sToFire -< ev
    returnA -< pl0 {playerPos = (P $ V2 x' y0), playerFiring = firing}

-- | Converts a raw input to a 'PlayerEvent'
toPlayerEvent :: Maybe Char -> PlayerEvent
toPlayerEvent Nothing = PNoevent
toPlayerEvent (Just c)
    | toLower c == 'a' = PMoveL
    | toLower c == 'd' = PMoveR
    | c == ' ' = PFire
    | otherwise = PNoevent

testPlayer2 :: IO ()
testPlayer2 = runBox clockSession_ b
    where
      pl0 = Player (P $ V2 30 28) False
      b = proc _ -> do
        pe <- arr toPlayerEvent <<< sInput -< ()
        pl' <- sUpdatePlayer pl0 -< pe
        mkKleisli_ putStr -< show pl' ++ "\r"
        returnA -< ()


-- $GameState-Bullets
-- 
-- After properly handling fire interval, we know that we have a logic
-- signal that denotes the status of firing. To actually fire a
-- bullet, we need some dynamic switching, so we will test this
-- section later when we have written collision handling.
--
-- First, we need 'Bullet' and 'BulletEvent' types. Since there are no
-- fancy special bullets here, we only need two internal states:
-- position and velocity. For this simple game, the only thing can be
-- done to a bullet is destroying. Similar to enemies, bullets has its
-- own system on velocity and position. Therefore, we are going to use
-- the 'integrate' signal and 'dPos' again. See 'sUpdateBullet' for
-- details. It looks basically like 'sUpdateEnemy0', so it is not hard
-- to grasp. Actually, it is perfectly fine to write a general version
-- like @sUpdateObject@ and write data class about destructable or
-- movable objects, but I won't go as far since the bullet will be the
-- only object with this implementation. That is why the previous
-- function is called 'sUpdateEnemy0': it will be replaced by another
-- implementation.

data Bullet = Bullet {
      bulletPos :: Point V2 Int
    , bulletVel :: V2 Int
    , bulletIsDestroyed :: Bool
    }

instance Show Bullet where
  show b = "[Bullet] Pos: "
           ++ show (x, y)
           ++ " Velocity: "
           ++ show (dx, dy)
      where
        P (V2 x y) = bulletPos b
        V2 dx dy = bulletVel b

data BulletEvent = BDestroy | BNoevent

sUpdateBullet :: (Monad m, HasTime t s) =>
                 Bullet
              -> Signal s m BulletEvent Bullet
sUpdateBullet b0 =
  let P p0 = fromIntegral <$> bulletPos b0
      dP0 = fromIntegral <$> bulletVel b0
  in proc ev -> do
    p' <- integrate p0 dPos -< dP0
    iP' <- arr (fmap round) -< p'
    d' <- latchS <<< arr (\case {BDestroy -> True; _ -> False}) -< ev
    returnA -< b0 {bulletPos = P iP', bulletIsDestroyed = d'}


-- $GameState-Enemy2
--
-- Now, our enemy has constant velocity. This is a problem: if we just
-- keep it in this way, the enemy will soon run off the screen. We
-- have to flip the horizontal direction when the enemy reaches the
-- boundary. This introduces a slight problem: our position depends on
-- velocity, and velocity depends on position. To solve this, we will
-- use the black magic of 'ArrowLoop', which somehow magically solves
-- this problem easily (I have read the book /Functional Reactive
-- Programming/, which uses Java 8 as the language. The circular
-- dependancy solution there seems a lot more complicated).
--
-- Since we are making the signals more complicated, it is a good idea
-- to further divide them. We observe that the position-velocity
-- system can be isolated away, so let's make a signal that only deals
-- with that. Take a look at 'sUpdateBoundedPosition'.
--
-- The type is long, but you may already guessed what it does. We pass
-- in the initial position, velocity, and boundary size. We get a
-- /constant/ signal that outputs the position of a point. I call it
-- /constant/ because it ignores any input. However, it does update
-- its state using the implicit time parameter.


sUpdateBoundedPosition :: (Monad m, HasTime t s) =>
                          Point V2 Double
                       -> V2 Double
                       -> Signal s m a (Point V2 Double)
sUpdateBoundedPosition p0 dP0 =
  proc _ -> do
    returnA -< p0 -- Placeholder
