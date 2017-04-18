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
  let left  = isScancodeDown (view leftKey player) keys
      right = isScancodeDown (view rightKey player) keys
      v | left && right = 0
        | left          = -playerSpeed
        | right         = playerSpeed
        | otherwise     = 0
  in  set xVel v player

jumpVelocity = -1000

moveWithGravity :: (HasTime t s, Moving o, Foldable f) => Wire s e m (f (ASetter' g o), g) g
moveWithGravity = mkPure $ \ds (setters, g) ->
  let dt = realToFrac $ dtime ds
  in  (Right $ moveWithGravityLenses dt setters g, moveWithGravity)

updatePlayerYV :: Position -> Keys -> Player -> Player
updatePlayerYV baseY keys player =
  let canJump = view yVel player == 0 && view yPos player >= baseY
      wantJump = isScancodeDown (view upKey player) keys
      player' = if canJump && wantJump then set yVel jumpVelocity player
                else player
  in player'

wrap :: Float -> Float -> Float -> Float
wrap mini maxi p
  | p > maxi  = mini + p - maxi
  | p < mini  = maxi - (mini - p)
  | otherwise = p

wallC = 1
groundC = 2 / 3
playerC = 1 / 5

updateBallA :: HasTime t s => Wire s e m GameScene GameScene
updateBallA = mkPure $ \ds scene ->
  let dt = realToFrac $ dtime ds
      scene' = over (ball . ballAFrame) (moveFrame dt) scene
  in (Right scene', updateBallA)

handleResetBall :: Keys -> GameScene -> GameScene
handleResetBall keys s@GameScene{_width=width, _height=height} =
  if isScancodePressed SDL.ScancodeN keys
    then over ball (resetBall width height) s
    else s

runBall :: (HasTime t s, Monad m) => Wire s () m (Keys, GameScene) GameScene
runBall =
  arr (uncurry handleResetBall) >>>
  arr (const [ball . ballObject]) &&& id >>>
  moveWithGravity >>>
  arr (collideLenses setRandomAV wallC ball [leftWall, rightWall, net] .
       collideLenses slowAV groundC ball [ground] .
       collideLenses id playerC ball [player1, player2]) >>>
  updateBallA
  where slowAV = over ballAV (* groundC)

handlePlayerInput :: Keys -> GameScene -> GameScene
handlePlayerInput keys scene =
  let playerBase = view baseY scene
      updatePlayerV = updatePlayerXV keys . updatePlayerYV playerBase keys
  in  over player1 updatePlayerV .
      over player2 updatePlayerV
      $ scene

runPlayers :: (HasTime t s, Monad m) => Wire s () m (Keys, GameScene) GameScene
runPlayers =
  arr (uncurry handlePlayerInput) >>>
  arr (const [player1 . playerObject, player2 . playerObject]) &&& id >>>
  moveWithGravity >>>
  arr (collideLenses id 1 player1 [leftWall, rightWall, net] .
       collideLenses id 1 player2 [leftWall, rightWall, net] .
       collideLenses (set yVel 0) 1 player1 [ground] .
       collideLenses (set yVel 0) 1 player2 [ground])


runClouds :: (HasTime t s, Monad m) => Wire s () m GameScene GameScene
runClouds = proc scene -> do
  scene' <- moveWithGravity -< ([clouds . traverse], scene)
  let w = view width scene
      base = view baseY scene
  returnA -< over clouds (map (over xPos (wrap (-w/2) w))) scene'

logic :: (HasTime t s, Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  scene1 <- runPlayers -< (keys, scene)
  scene2 <- runBall -< (keys, scene1)
  runClouds -< scene2

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
