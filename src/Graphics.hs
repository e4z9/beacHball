{-# LANGUAGE TemplateHaskell #-}

module Graphics where

import qualified Codec.Picture as I
import Control.Lens
import Control.Monad.IO.Class
import Control.Wire
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.C.Types
import qualified SDL
import Prelude hiding ((.))

data Sprite = Sprite {
  _texture :: SDL.Texture,
  _x :: Float,
  _y :: Float
}
makeLenses ''Sprite

class Scene s where
  renderScene :: MonadIO m => s -> (Sprite -> m ()) -> m ()

byteStringFromVector :: V.Vector Word8 -> B.ByteString
byteStringFromVector = B.pack . V.toList

loadTexture :: MonadIO m => SDL.Renderer -> FilePath -> m SDL.Texture
loadTexture renderer path = do
  maybeImage <- liftIO $ I.readImage path
  let image = either (\_ -> I.Image 0 0 mempty :: I.Image I.PixelRGBA8)
                     I.convertRGBA8 maybeImage
      w = fromIntegral $ I.imageWidth image
      h = fromIntegral $ I.imageHeight image
  -- no idea why, but updating texture in ABGR format takes the bytes in RGBA
  tex <- SDL.createTexture renderer SDL.ABGR8888 SDL.TextureAccessStreaming (SDL.V2 w h)
  tex2 <- SDL.updateTexture tex Nothing (byteStringFromVector (I.imageData image)) (4 * w)
  SDL.textureBlendMode tex2 SDL.$= SDL.BlendAlphaBlend
  return tex2

renderSprite :: MonadIO m => SDL.Renderer -> Sprite -> m ()
renderSprite r sprite = do
  let xi = round $ view x sprite
  let yi = round $ view y sprite
  let tex = view texture sprite
  textureInfo <- SDL.queryTexture tex
  let wi = SDL.textureWidth textureInfo
  let hi = SDL.textureHeight textureInfo
  SDL.copy r tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 xi yi)) (SDL.V2 wi hi)

render :: (MonadIO m, Scene s) => SDL.Renderer -> s -> m ()
render r scene = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 255 255 255 255
  SDL.clear r
  renderScene scene $ renderSprite r
  SDL.present r

renderLoop :: (HasTime t s, Monoid e, MonadIO m, Scene sc) => SDL.Renderer -> sc -> Session m s -> Wire s e m (sc, [SDL.Event]) sc -> m ()
renderLoop renderer scene session wire = do
  events <- SDL.pollEvents
  (step, session') <- stepSession session
  (output, wire') <- stepWire wire step $ Right (scene, events)
  either (const (pure ())) (\scene' -> do
      render renderer scene'
      renderLoop renderer scene' session' wire')
    output
