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

data Anchor =
  AnchorTopLeft |
  AnchorTopMid |
  AnchorTopRight |
  AnchorMidLeft |
  AnchorCenter |
  AnchorMidRight |
  AnchorBottomLeft |
  AnchorBottomMid |
  AnchorBottomRight

class Located p where
  xPos :: Lens' p Float
  yPos :: Lens' p Float

data SpriteTransform = SpriteTransform {
  _transformAngle :: Float,
  _transformFlip :: (Bool, Bool)
}
makeLenses ''SpriteTransform

data Sprite = Sprite {
  _texture :: SDL.Texture,
  _anchor :: Anchor,
  _x :: Float,
  _y :: Float,
  _w :: Int,
  _h :: Int,
  _spriteTransform :: Maybe SpriteTransform
}
makeLenses ''Sprite

instance Located Sprite where
  xPos = x
  yPos = y

class Scene s where
  renderScene :: MonadIO m => s -> (Sprite -> m ()) -> m ()
  clearColor :: s -> SDL.V4 Word8

createSprite :: MonadIO m => SDL.Renderer -> FilePath -> m Sprite
createSprite renderer texturePath = do
  (texture, w, h) <- loadTexture renderer texturePath
  return $ Sprite texture AnchorCenter 0 0 w h Nothing

spriteTopLeft :: Sprite -> (Float, Float)
spriteTopLeft sprite =
  let xs = view x sprite
      ys = view y sprite
      ws = fromIntegral $ view w sprite
      hs = fromIntegral $ view h sprite
  in case view anchor sprite of
    AnchorTopLeft     -> (xs, ys)
    AnchorTopMid      -> (xs - ws / 2, ys)
    AnchorTopRight    -> (xs - ws, ys)
    AnchorMidLeft     -> (xs, ys - hs / 2)
    AnchorCenter      -> (xs - ws / 2, ys - hs / 2)
    AnchorMidRight    -> (xs - ws, ys - hs / 2)
    AnchorBottomLeft  -> (xs, ys - hs)
    AnchorBottomMid   -> (xs - ws / 2, ys - hs)
    AnchorBottomRight -> (xs - ws, ys - hs)

byteStringFromVector :: V.Vector Word8 -> B.ByteString
byteStringFromVector = B.pack . V.toList

loadTexture :: MonadIO m => SDL.Renderer -> FilePath -> m (SDL.Texture, Int, Int)
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
  return (tex2, fromIntegral w, fromIntegral h)

renderSprite :: MonadIO m => SDL.Renderer -> Sprite -> m ()
renderSprite r sprite = do
  let (xp, yp) = spriteTopLeft sprite
      xi = fromIntegral $ round xp
      yi = fromIntegral $ round yp
      tex = view texture sprite
      wi = fromIntegral $ view w sprite
      hi = fromIntegral $ view h sprite
      targetRect = Just $ SDL.Rectangle (SDL.P (SDL.V2 xi yi)) (SDL.V2 wi hi)
      trans = view spriteTransform sprite
  maybe
    (SDL.copy r tex Nothing targetRect)
    (\t -> SDL.copyEx r tex Nothing targetRect
      (realToFrac $ view transformAngle t) Nothing
      (uncurry SDL.V2 $ view transformFlip t))
    trans

render :: (MonadIO m, Scene s) => SDL.Renderer -> s -> m ()
render r scene = do
  SDL.rendererDrawColor r SDL.$= clearColor scene
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
