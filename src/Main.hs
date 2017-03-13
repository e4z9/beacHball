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
import Prelude hiding ((.), id)

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

moveWithGravity :: (HasTime t s, Moving o, Foldable f) => Wire s e m (f (ASetter' g o), g) g
moveWithGravity = mkPure $ \ds (setters, g) ->
  let dt = realToFrac $ dtime ds
  in  (Right $ moveWithGravityLenses dt setters g, moveWithGravity)

updatePlayerYV :: Position -> Keys -> Player -> Player
updatePlayerYV baseY keys player =
  let canJump = view yVel player == 0 && view yPos player >= baseY
      wantJump = isScancodePressed (view upKey player) keys
      player' = if canJump && wantJump then set yVel jumpVelocity player
                else player
  in player'

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

wallC = 1
groundC = 2 / 3
playerC = 1 / 5

handleBallCollision :: GameScene -> GameScene
handleBallCollision = collideLenses setRandomAV wallC ball [leftWall, rightWall] .
                      collideLenses slowAV groundC ball [ground] .
                      collideLenses id playerC ball [player1, player2]
  where slowAV = over ballAV (* groundC)

updateBall :: HasTime t s => Wire s e m GameScene GameScene
updateBall = mkPure $ \ds scene ->
  let dt = realToFrac $ dtime ds
      scene' = over (ball . ballAFrame) (moveFrame dt) .
               handleBallCollision $ scene
  in (Right scene', updateBall)

handleInput :: Keys -> GameScene -> GameScene
handleInput keys scene =
  let playerBase = view baseY scene
      updatePlayerV = updatePlayerXV keys . updatePlayerYV playerBase keys
  in  over player1 updatePlayerV .
      over player2 updatePlayerV $ scene

logic :: (HasTime t s, Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  let scene1 = handleInput keys scene
  scene2 <- moveWithGravity -< ([player1, player2], scene1)
  scene3 <- moveWithGravity -< ([clouds . traverse], scene2)
  scene4 <- moveWithGravity -< ([ball], scene3)
  let w = view width startScene
      base = view baseY startScene
      scene5 = over player1 (restrictPlayerPos base) .
               over player2 (restrictPlayerPos base) .
               over clouds (map (over xPos (wrap (-w/2) w)))
               $ scene4
  updateBall -< scene5

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
