{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics
import Input

import Paths_beacHball

import Control.Lens
import Control.Monad.IO.Class
import Control.Wire
import FRP.Netwire
import qualified SDL
import System.Random
import Prelude hiding ((.))

type Velocity = Float
type Position = Float

data Player = Player {
  _leftKey :: SDL.Scancode,
  _rightKey :: SDL.Scancode,
  _upKey :: SDL.Scancode,
  _playerYV :: Float,
  _playerSprite :: Sprite
}
makeLenses ''Player

playerX :: Lens' Player Float
playerX = playerSprite . x

playerY :: Lens' Player Float
playerY = playerSprite . y

data Cloud = Cloud {
  _cloudXV :: Float,
  _cloudSprite :: Sprite
}
makeLenses ''Cloud

cloudX :: Lens' Cloud Float
cloudX = cloudSprite . x

data GameScene = GameScene {
  _width :: Float,
  _height :: Float,
  _background :: Sprite,
  _clouds :: [Cloud],
  _player1 :: Player,
  _player2 :: Player
}
makeLenses ''GameScene

instance Scene GameScene where
  renderScene s f = do
    f $ view background s
    mapM_ (f . view cloudSprite) $ view clouds s
    f $ view (player1 . playerSprite) s
    f $ view (player2 . playerSprite) s

playerSpeed :: Velocity
playerSpeed = 200

playerHSpeed :: (Monoid e, Monad m) => Wire s e m (Keys, Player) Velocity
playerHSpeed = mkSF_ $ \(keys, player) ->
  let left  = isScancodePressed (view leftKey player) keys
      right = isScancodePressed (view rightKey player) keys
  in  if left && right then 0
      else if left then (-playerSpeed)
      else if right then playerSpeed
      else 0

move :: HasTime t s => Wire s e m (Position, Velocity) Position
move = mkPure $ \ds (p, v) ->
  let dt = realToFrac $ dtime ds
  in (Right (p + v * dt), move)

bounded :: Ord a => a -> a -> Wire s e m a a
bounded mini maxi = mkSF_ $ \a ->
  if a <= mini then mini
  else if maxi <= a then maxi
  else a

playerHPos :: (HasTime t s, Monoid e, Monad m) => Position -> Position -> Wire s e m (Keys, Player) Player
playerHPos mini maxi = proc (keys, player) -> do
  v <- playerHSpeed -< (keys, player)
  nextx <- bounded mini maxi <<< move -< (view playerX player, v)
  returnA -< set playerX nextx player

player1HPos :: (HasTime t s, Monoid e, Monad m) => GameScene -> Wire s e m (Keys, Player) Player
player1HPos startScene =
  let halfScreen = view width startScene / 2
      halfSpriteWidth = fromIntegral (view (player1 . playerSprite . w) startScene) / 2
  in  playerHPos halfSpriteWidth (halfScreen - halfSpriteWidth)

player2HPos :: (HasTime t s, Monoid e, Monad m) => GameScene -> Wire s e m (Keys, Player) Player
player2HPos startScene =
  let halfScreen = view width startScene / 2
      halfSpriteWidth = fromIntegral (view (player1 . playerSprite . w) startScene) / 2
  in  playerHPos (halfScreen + halfSpriteWidth) (view width startScene - halfSpriteWidth)

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

moveCloudStep :: Float -> Float -> Cloud -> Cloud
moveCloudStep w dt cloud =
  let vel = view cloudXV cloud
      cx = view cloudX cloud
      cx' = if cx < w then cx else -w / 2
  in  set cloudX (cx' + vel * dt) cloud

moveClouds :: (HasTime t s, Monad m) => Float -> Wire s e m [Cloud] [Cloud]
moveClouds w = mkPure $ \ds clouds ->
  let dt = realToFrac $ dtime ds
  in  (Right $ map (moveCloudStep w dt) clouds, moveClouds w)

createPlayer1 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer1 r w h = do
  sprite <- createSprite r =<< getDataFileName "potato_sml.png"
  let player = Player SDL.ScancodeA SDL.ScancodeD SDL.ScancodeW 0 sprite
  return $ set playerX (w / 4) .
           set playerY (h - h / 5)
           $ player

createPlayer2 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer2 r w h = do
  sprite <- createSprite r =<< getDataFileName "potato_sml2.png"
  let player = Player SDL.ScancodeLeft SDL.ScancodeRight SDL.ScancodeUp 0 sprite
  return $ set playerX (w - w / 4) .
           set playerY (h - h / 5)
           $ player

createBackground :: SDL.Renderer -> Float -> Float -> IO Sprite
createBackground r w h = do
  (tex, twi, thi) <- loadTexture r =<< getDataFileName "background.png"
  let xp = w / 2
      yp = h
      tw = fromIntegral twi :: Float
      th = fromIntegral thi :: Float
      (sw, sh) = if w / h > tw / th then (round w, round (th * w / tw))
                 else (round (tw * h / th), round h)
  return $ Sprite tex AnchorBottomMid xp yp sw sh

createCloud :: SDL.Renderer -> Float -> Float -> FilePath -> IO Cloud
createCloud r w h path = do
  sprite <- createSprite r path
  xp <- randomRIO (- w / 2, w)
  yp <- randomRIO (0, h / 4)
  v <- randomRIO (2, 40)
  return $ Cloud v $ set x xp . set y yp . set anchor AnchorTopLeft $ sprite

createClouds :: SDL.Renderer -> Float -> Float -> IO [Cloud]
createClouds r w h = do
  paths <- mapM getDataFileName ["cloud" ++ show n ++ ".png" | n <- [1..5]]
  mapM (createCloud r w h) paths

startScene :: SDL.Window -> SDL.Renderer -> IO GameScene
startScene window renderer = do
  windowConfig <- SDL.getWindowConfig window
  let (SDL.V2 wi hi) = SDL.windowInitialSize windowConfig
      width = fromIntegral wi
      height = fromIntegral hi
  p1 <- createPlayer1 renderer width height
  p2 <- createPlayer2 renderer width height
  bg <- createBackground renderer width height
  clouds <- createClouds renderer width height
  return $ GameScene width height bg clouds p1 p2

logic :: (HasTime t s, Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  p1 <- player1HPos startScene -< (keys, view player1 scene)
  p1' <- playerVPos (view (player1 . playerY) startScene) -< (keys, p1)
  p2 <- player2HPos startScene -< (keys, view player2 scene)
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
