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

moveWithGravity :: (HasTime t s, Moving o, Functor f) => Wire s e m (f o) (f o)
moveWithGravity = mkPure $ \ds os ->
  let dt = realToFrac $ dtime ds
  in  (Right $ fmap (moveWithGravityStep dt) os, moveWithGravity)

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

wallCoefficient = 1

bounceWalls :: GameScene -> Ball -> Ball
bounceWalls scene ball = foldr (handleCollisionEx setRandomAV wallCoefficient)
                         ball [view leftWall scene, view rightWall scene]

groundCoefficient = 2 / 3

bounceGround :: GameScene -> Ball -> Ball
bounceGround scene = handleCollisionEx (over ballAV (* groundCoefficient))
                                       groundCoefficient (view ground scene)

playerCoefficient = 1 / 5

bouncePlayers :: GameScene -> Ball -> Ball
bouncePlayers scene = handleCollision playerCoefficient (view player1 scene) .
                      handleCollision playerCoefficient (view player2 scene)

handleBallCollision :: GameScene -> Ball -> Ball
handleBallCollision scene = bounceWalls scene . bounceGround scene .
                            bouncePlayers scene

updateBall :: HasTime t s => Wire s e m GameScene GameScene
updateBall = mkPure $ \ds scene ->
  let dt = realToFrac $ dtime ds
      scene' = over (ball . ballAFrame) (moveFrame dt) .
               over ball (handleBallCollision scene)
               $ scene
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
  let scene' = handleInput keys scene
  [p1', p2'] <- moveWithGravity -< [view player1 scene', view player2 scene']
  clouds' <- moveWithGravity -< view clouds scene'
  [ball'] <- moveWithGravity -< [view ball scene']
  let p1'' = restrictPlayerPos (view baseY startScene) p1'
      p2'' = restrictPlayerPos (view baseY startScene) p2'
      w = view width startScene
      clouds'' = map (over xPos (wrap (-w/2) w)) clouds'
  scene'' <- updateBall -< set ball ball' scene
  returnA -< set player2 p2'' .
             set player1 p1'' .
             set clouds clouds''
             $ scene''

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
