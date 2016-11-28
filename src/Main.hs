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

data GameScene = GameScene {
  _width :: Float,
  _height :: Float,
  _player1 :: Sprite,
  _player2 :: Sprite
}
makeLenses ''GameScene

instance Scene GameScene where
  renderScene s f = f (view player1 s) >> f (view player2 s)

playerSpeed :: Velocity
playerSpeed = 200

playerHSpeed :: (Monoid e, Monad m) => SDL.Scancode -> SDL.Scancode -> Wire s e m Keys Velocity
playerHSpeed left right = pure 0 . scancodePressed left . scancodePressed right
      <|> pure (-playerSpeed) . scancodePressed left
      <|> pure playerSpeed . scancodePressed right
      <|> pure 0

playerHMove :: HasTime t s => Wire s e m (Position, Velocity) Position
playerHMove = mkPure $ \ds (p, v) ->
  let dt = realToFrac $ dtime ds
  in (Right (p + v * dt), playerHMove)

bounded :: Ord a => a -> a -> Wire s e m a a
bounded mini maxi = mkSF_ $ \a ->
  if a <= mini then mini
  else if maxi <= a then maxi
  else a

playerHPos :: (HasTime t s, Monoid e, Monad m) => Position -> Position -> SDL.Scancode -> SDL.Scancode -> Wire s e m (Position, Keys) Position
playerHPos mini maxi left right = proc (p, keys) -> do
  v <- playerHSpeed left right -< keys
  bounded mini maxi <<< playerHMove -< (p, v)

player1HPos :: (HasTime t s, Monoid e, Monad m) => GameScene -> Wire s e m (Position, Keys) Position
player1HPos startScene =
  let halfScreen = view width startScene / 2
      spriteWidth = fromIntegral $ view (player1 . w) startScene
  in  playerHPos 0 (halfScreen - spriteWidth) SDL.ScancodeA SDL.ScancodeD

player2HPos :: (HasTime t s, Monoid e, Monad m) => GameScene -> Wire s e m (Position, Keys) Position
player2HPos startScene =
  let halfScreen = view width startScene / 2
      spriteWidth = fromIntegral $ view (player1 . w) startScene
  in  playerHPos halfScreen (view width startScene - spriteWidth) SDL.ScancodeLeft SDL.ScancodeRight

startScene :: SDL.Window -> SDL.Renderer -> IO GameScene
startScene window renderer = do
  windowConfig <- SDL.getWindowConfig window
  p1 <- createSprite renderer =<< getDataFileName "potato_sml.png"
  p2 <- createSprite renderer =<< getDataFileName "potato_sml2.png"
  let (SDL.V2 wi hi) = SDL.windowInitialSize windowConfig
      width = fromIntegral wi
      height = fromIntegral hi
      playerVPos = height - height / 4 - fromIntegral (view h p1) / 2
      wHalf = fromIntegral (view w p1) / 2
  return $ set (player1 . x) (width / 4 - wHalf) .
           set (player1 . y) playerVPos .
           set (player2 . x) (width - width / 4 - wHalf) .
           set (player2 . y) playerVPos
           $ GameScene width height p1 p2

logic :: (HasTime t s, Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  x1 <- player1HPos startScene -< (view (player1 . x) scene, keys)
  x2 <- player2HPos startScene -< (view (player2 . x) scene, keys)
  returnA -< set (player2 . x) x2 . set (player1 . x) x1 $ scene

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
