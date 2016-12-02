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
import Prelude hiding ((.))

type Velocity = Float
type Position = Float

data Player = Player {
  _leftKey :: SDL.Scancode,
  _rightKey :: SDL.Scancode,
  _upKey :: SDL.Scancode,
  _playerSprite :: Sprite
}
makeLenses ''Player

playerX :: Lens' Player Float
playerX = playerSprite . x

playerY :: Lens' Player Float
playerY = playerSprite . y

data GameScene = GameScene {
  _width :: Float,
  _height :: Float,
  _player1 :: Player,
  _player2 :: Player
}
makeLenses ''GameScene

instance Scene GameScene where
  renderScene s f = do
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

jump :: (HasTime t s, Fractional t, Monoid e, Monad m) => Wire s e m (Keys, Player) Player
jump = proc (_, player) -> do
  v <- for 0.3 . mkConst (Right (-800)) --> for 0.3 . mkConst (Right 800) -< ()
  nexty <- move -< (view playerY player, v)
  returnA -< set playerY nexty player

playerVPos :: (HasTime t s, Fractional t, Monoid e, Monad m) => Position -> Wire s e m (Keys, Player) Player
playerVPos startPos = switch (setFixedY &&& jumpSwitch) --> playerVPos startPos
  where
    setFixedY = mkSF_ $ \(_, player) -> set playerY startPos player
    jumpSwitch = (fmap (const jump) <$> scancodeTriggered) <<< getUpKey
    getUpKey = fmap (view upKey) <$> returnA

startScene :: SDL.Window -> SDL.Renderer -> IO GameScene
startScene window renderer = do
  windowConfig <- SDL.getWindowConfig window
  ps1 <- createSprite renderer =<< getDataFileName "potato_sml.png"
  ps2 <- createSprite renderer =<< getDataFileName "potato_sml2.png"
  let (SDL.V2 wi hi) = SDL.windowInitialSize windowConfig
      width = fromIntegral wi
      height = fromIntegral hi
      p1 = Player SDL.ScancodeA SDL.ScancodeD SDL.ScancodeW ps1
      p2 = Player SDL.ScancodeLeft SDL.ScancodeRight SDL.ScancodeUp ps2
      playerVPos = height - height / 4
  return $ set (player1 . playerX) (width / 4) .
           set (player1 . playerY) playerVPos .
           set (player2 . playerX) (width - width / 4) .
           set (player2 . playerY) playerVPos
           $ GameScene width height p1 p2

logic :: (HasTime t s, Fractional t,  Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  p1 <- player1HPos startScene -< (keys, view player1 scene)
  p1' <- playerVPos (view (player1 . playerY) startScene) -< (keys, p1)
  p2 <- player2HPos startScene -< (keys, view player2 scene)
  p2' <- playerVPos (view (player2 . playerY) startScene) -< (keys, p2)
  returnA -< set player2 p2' .
             set player1 p1'
             $ scene

anyRenderingDriver = -1

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "BeacHball" (SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1280 720 })
  let vsyncRenderer = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  renderer <- SDL.createRenderer window anyRenderingDriver vsyncRenderer
  scene <- startScene window renderer
  render renderer scene
  renderLoop renderer scene clockSession_ (logic scene)
  SDL.quit
