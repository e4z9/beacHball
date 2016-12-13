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

playerHSpeed :: Keys -> Player -> Velocity
playerHSpeed keys player =
  let left  = isScancodePressed (view leftKey player) keys
      right = isScancodePressed (view rightKey player) keys
  in  if left && right then 0
      else if left then (-playerSpeed)
      else if right then playerSpeed
      else 0

moveByVelocity :: Float -> Velocity -> Position -> Position
moveByVelocity dt v p = p + v * dt

bounded :: Ord a => a -> a -> a -> a
bounded mini maxi a
  | a <= mini = mini
  | maxi <= a = maxi
  | otherwise = a

playerHPos :: HasTime t s => Wire s e m (Keys, Player) Player
playerHPos = mkPure $ \ds (keys, player) ->
  let dt = realToFrac $ dtime ds
      v = playerHSpeed keys player
      (minX, maxX) = view playerHBounds player
      nextx = bounded minX maxX . moveByVelocity dt v
  in  (Right (over playerX nextx player), playerHPos)

gravitation = 2500
jumpVelocity = -1000

applyGravitation :: HasTime t s => Wire s e m (Position, Velocity) (Position, Velocity)
applyGravitation = mkPure $ \ds (p, v) ->
  let dt = realToFrac $ dtime ds
      nextv = v + gravitation * dt
      nextp = p + (v + nextv) / 2 * dt -- trapezoidal rule
  in (Right (nextp, nextv), applyGravitation)

playerVPos :: (HasTime t s, Monoid e, Monad m) => Position -> Wire s e m (Keys, Player) Player
playerVPos baseY = proc (keys, player) -> do
  let canJump = view playerYV player == 0 && view playerY player >= baseY
      wantJump = isScancodePressed (view upKey player) keys
      player' = if canJump && wantJump then set playerYV jumpVelocity player
                else player
  (nexty, nextyv) <- applyGravitation -< (view playerY player', view playerYV player')
  let (nexty', nextyv') = if nexty >= baseY then (baseY, 0)
                          else (nexty, nextyv)
  returnA -< set playerY nexty' . set playerYV nextyv' $ player'

wrap :: Float -> Float -> Float -> Float
wrap mini maxi p
  | p > maxi  = mini + p - maxi
  | p < mini  = maxi - (mini - p)
  | otherwise = p

moveCloudStep :: Float -> Float -> Cloud -> Cloud
moveCloudStep w dt cloud =
  let vel = view cloudXV cloud
  in  over cloudX (wrap (-w/2) w . moveByVelocity dt vel) cloud

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
