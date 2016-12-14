{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Graphics
import Input
import Scene

import Control.Lens
import Control.Wire
import FRP.Netwire
import qualified SDL
import Prelude hiding ((.))

type Velocity = Float
type Position = Float

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
  in  set playerXV v player

moveStep :: Float -> (Position, Velocity) -> (Position, Velocity)
moveStep dt (p, v) = (p + v * dt, v)

bounded :: Ord a => a -> a -> a -> a
bounded mini maxi a
  | a <= mini = mini
  | maxi <= a = maxi
  | otherwise = a

playerHPos :: HasTime t s => Wire s e m (Keys, Player) Player
playerHPos = mkPure $ \ds (keys, player) ->
  let dt = realToFrac $ dtime ds
      (minX, maxX) = view playerHBounds player
      player' = over playerX (bounded minX maxX) .
                over playerXFrame (moveStep dt) .
                updatePlayerXV keys $ player
  in  (Right player', playerHPos)

gravity = 2500
jumpVelocity = -1000

moveWithGravityStep :: Float -> Float -> (Position, Velocity) -> (Position, Velocity)
moveWithGravityStep gravity dt (p, v) =
  let (v', _) = moveStep dt (v, gravity)
      p' = p + (v + v') / 2 * dt -- trapezoidal rule
  in  (p', v')

playerVPos :: HasTime t s => Position -> Wire s e m (Keys, Player) Player
playerVPos baseY = mkPure $ \ds (keys, player) ->
  let dt = realToFrac $ dtime ds
      canJump = view playerYV player == 0 && view playerY player >= baseY
      wantJump = isScancodePressed (view upKey player) keys
      player' = if canJump && wantJump then set playerYV jumpVelocity player
                else player
      restrictToBase (p, v) = if p >= baseY then (baseY, 0) else (p, v)
      player'' = over playerYFrame (restrictToBase . moveWithGravityStep gravity dt) player'
  in (Right player'', playerVPos baseY)

wrap :: Float -> Float -> Float -> Float
wrap mini maxi p
  | p > maxi  = mini + p - maxi
  | p < mini  = maxi - (mini - p)
  | otherwise = p

moveCloudStep :: Float -> Float -> Cloud -> Cloud
moveCloudStep w dt = over cloudX (wrap (-w/2) w) . over cloudXFrame (moveStep dt)

moveClouds :: HasTime t s => Float -> Wire s e m [Cloud] [Cloud]
moveClouds w = mkPure $ \ds clouds ->
  let dt = realToFrac $ dtime ds
  in  (Right $ map (moveCloudStep w dt) clouds, moveClouds w)

logic :: (HasTime t s, Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  p1 <- playerHPos -< (keys, view player1 scene)
  p1' <- playerVPos (view (player1 . playerY) startScene) -< (keys, p1)
  p2 <- playerHPos -< (keys, view player2 scene)
  p2' <- playerVPos (view (player2 . playerY) startScene) -< (keys, p2)
  clouds' <- moveClouds (view width startScene) -< view clouds scene
  returnA -< set player2 p2' .
             set player1 p1' .
             set clouds clouds'
             $ scene

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
