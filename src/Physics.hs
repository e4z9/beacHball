{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Physics where

import Graphics

import Control.Arrow
import Control.Lens

type Velocity = Float
type Radius = Float
type Circle = ((Position, Position), Radius) -- mid point, radius
type Line = ((Position, Position), (Position, Position)) -- point, normal

data CollisionShape =
  CollisionNone |
  CollisionCircle Circle |
  CollisionLine Line
  deriving Show

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
  collisionShape :: m -> CollisionShape
  collisionShape = const CollisionNone

data Object = Object {
  _objItem :: GraphicsItem,
  _objXVel :: Velocity,
  _objYVel :: Velocity,
  _objGravity :: Float,
  _objCollisionShape :: Object -> CollisionShape
}
makeLenses ''Object

object :: Object
object = Object graphicsItem 0 0 0 (const CollisionNone)

instance Located Object where
  xPos = objItem . xPos
  yPos = objItem . yPos

instance Moving Object where
  xVel = objXVel
  yVel = objYVel
  gravity = view objGravity
  collisionShape o = view objCollisionShape o o

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

moveWithGravityLenses :: (Foldable f, Moving o) => Float -> f (ASetter' s o) -> s -> s
moveWithGravityLenses dt setters s = foldr (`over` moveWithGravityStep dt) s setters

checkCollisionCircleLine :: Line -> Circle -> Maybe ((Float, Float), (Float, Float))
checkCollisionCircleLine ((lx, ly), (nx, ny)) ((cx, cy), cr) =
  if isColliding then Just ((nx, ny), (dx, dy))
                 else Nothing
  where
    -- vector pointing from line base and circle
    (lcx, lcy) = (cx - lx, cy - ly)
    -- normal component
    d = lcx * nx + lcy * ny
    isColliding = d < cr - 0.001
    correctionDistance = cr - d
    (dx, dy) = (correctionDistance * nx, correctionDistance * ny)

-- Collision happens if distance is smaller then sum of both radii.
-- Returns normal and correction values for o1 in case of collision.
checkCollisionCircleCircle :: Circle -> Circle -> Maybe ((Float, Float), (Float, Float))
checkCollisionCircleCircle ((x2, y2), r2) ((x1, y1), r1) =
  if isColliding then Just (normal, (dx, dy))
                 else Nothing
  where
    -- vector pointing from o2 to o1
    (dx21, dy21) = (x1 - x2, y1 - y2)
    rsum = r1 + r2
    sqrDist = dx21*dx21 + dy21*dy21
    d = sqrt sqrDist
    -- normal
    normal@(nx, ny) = if d > 0.001 then (dx21 / d, dy21 / d)
                                   else (0, -1) -- pathological case, just point up
    isColliding = sqrDist < rsum*rsum - 0.001
    -- move ball to outside of player along normal
    correctionDistance = rsum - d
    (dx, dy) = (correctionDistance * nx, correctionDistance * ny)

checkCollision :: (Moving o1, Moving o2) => o2 -> o1 -> Maybe ((Float, Float), o1)
checkCollision o2 o1 =
  case (collisionShape o1, collisionShape o2) of
    (CollisionCircle c, CollisionLine l) ->
      checkCollisionCircleLine l c >>= restrict
    (CollisionCircle c1, CollisionCircle c2) ->
      checkCollisionCircleCircle c2 c1 >>= restrict
    _ -> Nothing
  where
    restrict (normal, (dx, dy)) = return (normal, over xPos (+dx) . over yPos (+dy) $ o1)

bounce :: (Moving o1, Moving o2) => Float -> (Float, Float) -> o2 -> o1 -> o1
bounce coeff (nx, ny) o2 o1 = set xVel (nv1' * nx + tv1' * tx) .
                  set yVel (nv1' * ny + tv1' * ty) $ o1
  where
    -- Momentum is transported in normal direction only, tangential component
    -- stays same (except for energy loss described by coefficient).
    -- Mass of o2 is >> mass of o1.
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
    -- new o1 velocity
    nv1' = nv2 - (nv1 - nv2)*coeff
    tv1' = tv1 * coeff

collide :: (Moving o1, Moving o2) => (o1 -> o1) -> Float -> o2 -> o1 -> o1
collide callback coeff heavy light =
  case checkCollision heavy light of
    Just (normal, light') -> callback $ bounce coeff normal heavy light'
    Nothing               -> light

collideLenses :: (Foldable f, Moving o, Moving b) => (b -> b) -> Float -> ASetter' s b -> f (Getting o s o) -> s -> s
collideLenses callback coeff b getters s = over b updateB s
  where
    updateB b = foldr (collide callback coeff . flip view s) b getters
