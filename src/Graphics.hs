{-# LANGUAGE TemplateHaskell #-}

module Graphics where

import Control.Lens
import Control.Monad.IO.Class
import Control.Wire
import Foreign.C.Types
import qualified SDL

data Sprite = Sprite {
  _texture :: SDL.Texture,
  _x :: Float,
  _y :: Float
}
makeLenses ''Sprite

data Scene = Scene {
  _player1 :: Sprite,
  _player2 :: Sprite
}
makeLenses ''Scene

loadTexture :: MonadIO m => SDL.Renderer -> FilePath -> m SDL.Texture
loadTexture renderer path = do
  surface <- SDL.loadBMP path
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return texture

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
