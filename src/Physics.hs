{-# LANGUAGE RankNTypes #-}

module Physics where

import Graphics

import Control.Arrow
import Control.Lens

type Velocity = Float

frame :: Lens' m Position -> Lens' m Velocity -> Lens' m (Position, Velocity)
frame pos vel = lens (view pos &&& view vel)
                (\c (p, v) -> (set pos p . set vel v) c)

class Located m => Moving m where
  xVel :: Lens' m Velocity
  yVel :: Lens' m Velocity
  xFrame :: Lens' m (Position, Velocity)
  xFrame = frame xPos xVel
  yFrame :: Lens' m (Position, Velocity)
  yFrame = frame yPos yVel

moveFrame :: Float -> (Position, Velocity) -> (Position, Velocity)
moveFrame dt (p, v) = (p + v * dt, v)

moveFrameWithGravity :: Float -> Float -> (Position, Velocity) -> (Position, Velocity)
moveFrameWithGravity gravity dt (p, v) =
  let (v', _) = moveFrame dt (v, gravity)
      p' = p + (v + v') / 2 * dt -- trapezoidal rule
  in  (p', v')

moveWithGravityStep :: Moving o => Float -> Float -> o -> o
moveWithGravityStep gravity dt =
  over xFrame (moveFrame dt) .
  over yFrame (moveFrameWithGravity gravity dt)
