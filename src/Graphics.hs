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
import qualified SDL.Raw
import SDL.TTF as TTF
import SDL.TTF.FFI as TTF.FFI

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

type Position = Float

class Located p where
  xPos :: Lens' p Position
  yPos :: Lens' p Position

data SpriteTransform = SpriteTransform {
  _transformAngle :: Float,
  _transformFlip :: (Bool, Bool)
}
makeLenses ''SpriteTransform

data Sprite = Sprite {
  _texture :: SDL.Texture,
  _anchor :: Anchor,
  _w :: Int,
  _h :: Int,
  _spriteTransform :: Maybe SpriteTransform
}
makeLenses ''Sprite

data LineInfo = LineInfo (Position, Position) (SDL.V4 Word8) -- (dx, dy) color

data RenderItem =
  RenderSprite Sprite |
  RenderLine LineInfo
makePrisms ''RenderItem

data GraphicsItem = GraphicsItem {
  _itemX :: Position,
  _itemY :: Position,
  _itemRenderItem :: Maybe RenderItem
}
makeLenses ''GraphicsItem
itemSprite :: Traversal' GraphicsItem Sprite
itemSprite = itemRenderItem . _Just . _RenderSprite

instance Located GraphicsItem where
  xPos = itemX
  yPos = itemY

class Scene s where
  -- traverseItems_ is also Traversal s () GraphicsItem b
  traverseItems_ :: Applicative f => (GraphicsItem -> f b) -> s -> f ()
  clearColor :: s -> SDL.V4 Word8

graphicsItem :: GraphicsItem
graphicsItem = GraphicsItem 0 0 Nothing

graphicsSpriteItem :: MonadIO m => SDL.Renderer -> FilePath -> m GraphicsItem
graphicsSpriteItem renderer texturePath = do
  s <- Just . RenderSprite <$> sprite renderer texturePath
  return $ set itemRenderItem s graphicsItem

graphicsTextItem :: MonadIO m => SDL.Renderer -> TTF.FFI.TTFFont ->
                                 SDL.V4 Word8 -> String -> m GraphicsItem
graphicsTextItem renderer font color text = do
  s <- Just . RenderSprite <$> textSprite renderer font color text
  return $ set itemRenderItem s graphicsItem

sprite :: MonadIO m => SDL.Renderer -> FilePath -> m Sprite
sprite renderer texturePath = do
  (texture, w, h) <- loadTexture renderer texturePath
  return $ Sprite texture AnchorCenter w h Nothing

textSprite :: MonadIO m => SDL.Renderer -> TTF.FFI.TTFFont -> SDL.V4 Word8 -> String -> m Sprite
textSprite renderer font (SDL.V4 r g b a) text = do
  surface <- TTF.renderUTF8Blended font text (SDL.Raw.Color r g b a)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  texInfo <- SDL.queryTexture texture
  let w = fromIntegral $ SDL.textureWidth texInfo
      h = fromIntegral $ SDL.textureHeight texInfo
  return $ Sprite texture AnchorTopLeft w h Nothing

spriteTopLeft :: Position -> Position -> Sprite -> (Position, Position)
spriteTopLeft xs ys sprite =
  let ws = fromIntegral $ view w sprite
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

renderSprite :: MonadIO m => SDL.Renderer -> Position -> Position -> Sprite -> m ()
renderSprite r x y sprite =
  let (xp, yp) = spriteTopLeft x y sprite
      xi = fromIntegral $ round xp
      yi = fromIntegral $ round yp
      tex = view texture sprite
      wi = fromIntegral $ view w sprite
      hi = fromIntegral $ view h sprite
      targetRect = Just $ SDL.Rectangle (SDL.P (SDL.V2 xi yi)) (SDL.V2 wi hi)
      trans = view spriteTransform sprite
  in maybe
    (SDL.copy r tex Nothing targetRect)
    (\t -> SDL.copyEx r tex Nothing targetRect
      (realToFrac $ view transformAngle t) Nothing
      (uncurry SDL.V2 $ view transformFlip t))
    trans

renderLine :: MonadIO m => SDL.Renderer -> Position -> Position -> LineInfo -> m ()
renderLine r x y (LineInfo (dx, dy) color) = do
  let xi1 = fromIntegral $ round x
      yi1 = fromIntegral $ round y
      xi2 = fromIntegral $ round (x + dx)
      yi2 = fromIntegral $ round (y + dy)
  SDL.rendererDrawColor r SDL.$= color
  SDL.drawLine r (SDL.P (SDL.V2 xi1 yi1)) (SDL.P (SDL.V2 xi2 yi2))

renderItem :: MonadIO m => SDL.Renderer -> GraphicsItem -> m ()
renderItem r item = mapM_ renderActualItem (view itemRenderItem item)
  where
    x = view itemX item
    y = view itemY item
    renderActualItem (RenderSprite sprite) = renderSprite r x y sprite
    renderActualItem (RenderLine line)     = renderLine r x y line

render :: (MonadIO m, Scene s) => SDL.Renderer -> s -> m ()
render r scene = do
  SDL.rendererDrawColor r SDL.$= clearColor scene
  SDL.clear r
  traverseItems_ (renderItem r) scene
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
