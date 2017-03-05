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
  gravity :: m -> Float
  gravity = const 0

moveFrame :: Float -> (Position, Velocity) -> (Position, Velocity)
moveFrame dt (p, v) = (p + v * dt, v)

moveFrameWithGravity :: Float -> Float -> (Position, Velocity) -> (Position, Velocity)
moveFrameWithGravity gravity dt (p, v) =
  let (v', _) = moveFrame dt (v, gravity)
      p' = p + (v + v') / 2 * dt -- trapezoidal rule
  in  (p', v')

moveWithGravityStep :: Moving o => Float -> o -> o
moveWithGravityStep dt obj =
  over xFrame (moveFrame dt) .
  over yFrame (moveFrameWithGravity (gravity obj) dt) $ obj

bounce :: (Moving o1, Moving o2) => Float -> (Float, Float) -> o2 -> o1 -> o1
bounce coeff (nx, ny) o2 o1 = set xVel (nv1' * nx + tv1 * tx) .
                  set yVel (nv1' * ny + tv1 * ty) $ o1
  where
    -- Momentum is transported in normal direction only, tangential component
    -- stays same. Mass of o2 is >> mass of o1.
    -- Coefficient k (0-1) describes how much energy is preserved.
    -- Formula for resulting velocities (in normal direction):
    -- v1' = (m1*v1 + m2*v2 - m2*(v1 - v2)*k) / (m1 + m2)
    -- v2' = (m1*v1 + m2*v2 - m1*(v2 - v1)*k) / (m1 + m2)
    -- and with m1 << m2:
    -- v1' = v2 - (v1 - v2)*k
    -- v2' = v2
    -- tangent
    (tx, ty) = (-ny, nx)
    -- o1 (light object) velocity
    (xv1, yv1) = (view xVel o1, view yVel o1)
    (nv1, tv1) = (xv1 * nx + yv1 * ny, xv1 * tx + yv1 * ty) -- dot products
    -- o2 (heavy object) velocity
    (xv2, yv2) = (view xVel o2, view yVel o2)
    nv2 = xv2 * nx + yv2 * ny
    -- new o1 velocity in normal direction
    nv1' = nv2 - (nv1 - nv2)*coeff
