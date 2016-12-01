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
      halfSpriteWidth = fromIntegral (view (player1 . w) startScene) / 2
  in  playerHPos halfSpriteWidth (halfScreen - halfSpriteWidth) SDL.ScancodeA SDL.ScancodeD

player2HPos :: (HasTime t s, Monoid e, Monad m) => GameScene -> Wire s e m (Position, Keys) Position
player2HPos startScene =
  let halfScreen = view width startScene / 2
      halfSpriteWidth = fromIntegral (view (player1 . w) startScene) / 2
  in  playerHPos (halfScreen + halfSpriteWidth) (view width startScene - halfSpriteWidth) SDL.ScancodeLeft SDL.ScancodeRight

playerVPos :: (HasTime t s, Fractional t, Monoid e, Monad m) => SDL.Scancode -> Position -> Wire s e m Keys Position
playerVPos up startPos = switch ((,) startPos <$> jumpSwitch) --> playerVPos up startPos
  where
    jumpSwitch = fmap (const moveUp) <$> scancodeTriggered up
    moveUp = (for 0.3 . mkConst (Right (-800))
              --> for 0.3 . mkConst (Right 800)) >>> integral startPos

player1VPos :: (HasTime t s, Fractional t, Monoid e, Monad m) => Position -> Wire s e m Keys Position
player1VPos = playerVPos SDL.ScancodeW
player2VPos :: (HasTime t s, Fractional t, Monoid e, Monad m) => Position -> Wire s e m Keys Position
player2VPos = playerVPos SDL.ScancodeUp

startScene :: SDL.Window -> SDL.Renderer -> IO GameScene
startScene window renderer = do
  windowConfig <- SDL.getWindowConfig window
  p1 <- createSprite renderer =<< getDataFileName "potato_sml.png"
  p2 <- createSprite renderer =<< getDataFileName "potato_sml2.png"
  let (SDL.V2 wi hi) = SDL.windowInitialSize windowConfig
      width = fromIntegral wi
      height = fromIntegral hi
      playerVPos = height - height / 4
  return $ set (player1 . x) (width / 4) .
           set (player1 . y) playerVPos .
           set (player2 . x) (width - width / 4) .
           set (player2 . y) playerVPos
           $ GameScene width height p1 p2

logic :: (HasTime t s, Fractional t,  Monad m) => GameScene -> Wire s () m (GameScene, [SDL.Event]) GameScene
logic startScene = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  x1 <- player1HPos startScene -< (view (player1 . x) scene, keys)
  y1 <- player1VPos (view (player1 . y) startScene) -< keys
  x2 <- player2HPos startScene -< (view (player2 . x) scene, keys)
  y2 <- player2VPos (view (player2 . y) startScene) -< keys
  returnA -< set (player2 . x) x2 .
             set (player1 . x) x1 .
             set (player2 . y) y2 .
             set (player1 . y) y1
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
