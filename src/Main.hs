{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Graphics
import Input
import Physics
import Scene

import Control.Lens
import Control.Wire
import FRP.Netwire
import qualified SDL
import Prelude hiding ((.))

playerSpeed :: Velocity
playerSpeed = 200

updatePlayerXV :: Keys -> Player -> Player
updatePlayerXV keys player =
  let left  = isScancodePressed (view leftKey player) keys
      right = isScancodePressed (view rightKey player) keys
      v | left && right = 0
        | left          = -playerSpeed
        | right         = playerSpeed
        | otherwise     = 0
  in  set xVel v player

bounded :: Ord a => a -> a -> a -> a
bounded mini maxi a
  | a <= mini = mini
  | maxi <= a = maxi
  | otherwise = a

jumpVelocity = -1000

moveWithGravity :: (HasTime t s, Moving o) => Wire s e m o o
moveWithGravity = mkPure $ \ds o ->
  let dt = realToFrac $ dtime ds
  in (Right (moveWithGravityStep dt o), moveWithGravity)

updatePlayerYV :: Position -> Keys -> Player -> Player
updatePlayerYV baseY keys player =
  let canJump = view yVel player == 0 && view yPos player >= baseY
      wantJump = isScancodePressed (view upKey player) keys
      player' = if canJump && wantJump then set yVel jumpVelocity player
                else player
  in player'

updatePlayerV :: Float -> Keys -> Player -> Player
updatePlayerV baseY keys = updatePlayerXV keys . updatePlayerYV baseY keys

restrictPlayerPos :: Position -> Player -> Player
restrictPlayerPos baseY player =
  let (minX, maxX) = view playerHBounds player
      restrictToBase (p, v) = if p >= baseY then (baseY, 0) else (p, v)
  in over xPos (bounded minX maxX) .
     over yFrame restrictToBase $ player

wrap :: Float -> Float -> Float -> Float
wrap mini maxi p
  | p > maxi  = mini + p - maxi
  | p < mini  = maxi - (mini - p)
  | otherwise = p

moveCloudStep :: Float -> Float -> Cloud -> Cloud
moveCloudStep w dt = over xPos (wrap (-w/2) w) . moveWithGravityStep dt

moveClouds :: HasTime t s => Float -> Wire s e m [Cloud] [Cloud]
moveClouds w = mkPure $ \ds clouds ->
  let dt = realToFrac $ dtime ds
  in  (Right $ map (moveCloudStep w dt) clouds, moveClouds w)

bounceWalls :: GameScene -> Ball -> Ball
bounceWalls scene ball = bounceRight . bounceLeft $ ball
  where halfBall :: Float
        halfBall = fromIntegral (view (ballSprite . w) ball) / 2
        bounceLeft b = if view xPos b - halfBall > 0 then b
                       else setRandomAV $ set xPos halfBall . over xVel negate $ b
        sw = view width scene
        bounceRight b = if view xPos b + halfBall < sw  then b
                        else setRandomAV $ set xPos (sw - halfBall) . over xVel negate $ b

groundCoefficient = 2 / 3

bounceGround :: GameScene -> Ball -> Ball
bounceGround scene ball = if isBouncing then bounce ball else ball
  where
    base = view baseY scene - fromIntegral (view (ballSprite . h) ball) / 2
    isBouncing = view yPos ball > base
    bounce = set yPos base . over yVel (negate . (* groundCoefficient)) .
             over xVel (* groundCoefficient) .
             over ballAV (* groundCoefficient)

playerCoefficient = 1 / 5

bouncePlayer :: Player -> Ball -> Ball
bouncePlayer player ball = if isBouncing then bounce ball else ball
  where
    -- Collision happens if distance is smaller then sum of both radii.
    -- Momentum is transported in normal direction only, tangential component
    -- stays same. Mass of player is >> mass of ball.
    -- Coefficient k (0-1) describes how much energy is preserved.
    -- Formula for resulting velocities (in normal direction):
    -- v1' = (m1*v1 + m2*v2 - m2*(v1 - v2)*k) / (m1 + m2)
    -- v2' = (m1*v1 + m2*v2 - m1*(v2 - v1)*k) / (m1 + m2)
    -- and with m1 << m2:
    -- v1' = v2 - (v1 - v2)*k
    -- v2' = v2
    ((px, py), pr) = collisionCircle $ view playerSprite player
    ((bx, by), br) = collisionCircle $ view ballSprite ball
    -- vector pointing from player to ball
    (pbx, pby) = (bx - px, by - py)
    rsum = pr + br
    sqrDist = pbx*pbx + pby*pby
    d = sqrt sqrDist
    -- normal
    (nx, ny) = if d > 0.001 then (pbx / d, pby / d)
                            else (0, -1) -- pathological case, just point up
    -- tangent
    (tx, ty) = (-ny, nx)
    -- ball velocity
    (bxv, byv) = (view xVel ball, view yVel ball)
    (bnv, btv) = (bxv * nx + byv * ny, bxv * tx + byv * ty) -- dot products
    -- player velocity
    (pxv, pyv) = (view xVel player, view yVel player)
    pnv = pxv * nx + pyv * ny
    -- if near enough, and ball moves in direction of player:
    isBouncing = sqrDist < rsum*rsum - 0.001
    -- new ball velocity in normal direction
    bnv' = pnv - (bnv - pnv)*playerCoefficient
    -- move ball to outside of player along normal
    correctionDistance = rsum - d
    (dx, dy) = (correctionDistance * nx, correctionDistance * ny)
    bounce = set xVel (bnv' * nx + btv * tx) .
             set yVel (bnv' * ny + btv * ty) .
             over xPos (+dx) .
             over yPos (+dy)

bouncePlayers :: GameScene -> Ball -> Ball
bouncePlayers scene = bouncePlayer (view player1 scene) .
                      bouncePlayer (view player2 scene)

handleBallCollision :: GameScene -> Ball -> Ball
handleBallCollision scene = bounceWalls scene . bounceGround scene .
                            bouncePlayers scene

updateBall :: HasTime t s => Wire s e m GameScene GameScene
updateBall = mkPure $ \ds scene ->
  let dt = realToFrac $ dtime ds
      scene' = over (ball . ballAFrame) (moveFrame dt) .
               over ball (handleBallCollision scene) .
               over ball (moveWithGravityStep dt)
               $ scene
  in (Right scene', updateBall)

logic :: (HasTime t s, Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  let p1 = updatePlayerV (view baseY startScene) keys (view player1 scene)
  p1' <- moveWithGravity -< p1
  let p1'' = restrictPlayerPos (view baseY startScene) p1'
  let p2 = updatePlayerV (view baseY startScene) keys (view player2 scene)
  p2' <- moveWithGravity -< p2
  let p2'' = restrictPlayerPos (view baseY startScene) p2'
  clouds' <- moveClouds (view width startScene) -< view clouds scene
  scene' <- updateBall -< scene
  returnA -< set player2 p2'' .
             set player1 p1'' .
             set clouds clouds'
             $ scene'

anyRenderingDriver = -1

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "BeacHball" (SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1280 820 })
  let vsyncRenderer = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  renderer <- SDL.createRenderer window anyRenderingDriver vsyncRenderer
  scene <- startScene window renderer
  render renderer scene
  renderLoop renderer scene clockSession_ (logic scene)
  SDL.quit
