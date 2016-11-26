{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Input

import Paths_beacHball

import Control.Lens
import Control.Monad.IO.Class
import Control.Wire
import Foreign.C.Types
import FRP.Netwire
import qualified SDL
import Prelude hiding ((.))

playerSpeed :: CFloat
playerSpeed = 200

speedControl :: (Monoid e, Monad m) => SDL.Scancode -> SDL.Scancode -> Wire s e m Keys CFloat
speedControl left right = pure 0 . scancodePressed left . scancodePressed right
      <|> pure (-playerSpeed) . scancodePressed left
      <|> pure playerSpeed . scancodePressed right
      <|> pure 0

pos1 :: (HasTime t s, Monoid e, Monad m) => Wire s e m Keys CFloat
pos1 = integral 50 . speedControl SDL.ScancodeA SDL.ScancodeD

pos2 :: (HasTime t s, Monoid e, Monad m) => Wire s e m Keys CFloat
pos2 = integral 350 . speedControl SDL.ScancodeLeft SDL.ScancodeRight

loadTexture :: MonadIO m => SDL.Renderer -> FilePath -> m SDL.Texture
loadTexture renderer path = do
  surface <- SDL.loadBMP path
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return texture

data Sprite = Sprite {
  _texture :: SDL.Texture,
  _x :: CFloat,
  _y :: CFloat
}
makeLenses ''Sprite

data Scene = Scene {
  _player1 :: Sprite,
  _player2 :: Sprite
}
makeLenses ''Scene

startScene :: SDL.Renderer -> IO Scene
startScene renderer = do
  potato1 <- loadTexture renderer =<< getDataFileName "potato_sml.bmp"
  potato2 <- loadTexture renderer =<< getDataFileName "potato_sml2.bmp"
  return $ Scene (Sprite potato1 50 250) (Sprite potato2 350 250)

logic :: (HasTime t s, Monad m) => Wire s () m (Scene, [SDL.Event]) Scene
logic = proc (scene, events) -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  x1 <- pos1 -< keys
  x2 <- pos2 -< keys
  returnA -< set (player2 . x) x2 . set (player1 . x) x1 $ scene

anyRenderingDriver = -1

toCInt n = CInt (fromInteger $ toInteger $ round n)

renderSprite :: MonadIO m => SDL.Renderer -> Sprite -> m ()
renderSprite r sprite = do
  let xi = toCInt $ view x sprite
  let yi = toCInt $ view y sprite
  let tex = view texture sprite
  textureInfo <- SDL.queryTexture tex
  let wi = SDL.textureWidth textureInfo
  let hi = SDL.textureHeight textureInfo
  SDL.copy r tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 xi yi)) (SDL.V2 wi hi)

render :: MonadIO m => SDL.Renderer -> Scene -> m ()
render r scene = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 255 255 255 255
  SDL.clear r
  renderSprite r $ view player1 scene
  renderSprite r $ view player2 scene
  SDL.present r

renderLoop :: (HasTime t s, Monoid e, MonadIO m) => SDL.Renderer -> Scene -> Session m s -> Wire s e m (Scene, [SDL.Event]) Scene -> m ()
renderLoop renderer scene session wire = do
  events <- SDL.pollEvents
  (step, session') <- stepSession session
  (output, wire') <- stepWire wire step $ Right (scene, events)
  either (const (pure ())) (\scene' -> do
      render renderer scene'
      renderLoop renderer scene' session' wire')
    output

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "BeacHball" SDL.defaultWindow
  let vsyncRenderer = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  renderer <- SDL.createRenderer window anyRenderingDriver vsyncRenderer
  scene <- startScene renderer
  renderLoop renderer scene clockSession_ logic
  SDL.quit
