{-# LANGUAGE TemplateHaskell #-}

module Graphics where

import qualified Codec.Picture as I
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Control.Wire as W
import qualified Data.ByteString as B
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.C.Types
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Font as TTF

import Prelude

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
  _itemRenderItem :: Maybe RenderItem,
  _itemVisible :: Bool
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

data RenderEnv = RenderEnv {
  _originX :: Position,
  _originY :: Position,
  renderer :: SDL.Renderer
}
makeLenses ''RenderEnv

renderEnv :: SDL.Renderer -> RenderEnv
renderEnv = RenderEnv 0 0

type RenderContext m a = ReaderT RenderEnv m a

askOrigin :: Monad m => RenderContext m (Position, Position)
askOrigin = asks $ _originX &&& _originY

graphicsItem :: GraphicsItem
graphicsItem = GraphicsItem {
  _itemX = 0,
  _itemY = 0,
  _itemRenderItem = Nothing,
  _itemVisible = True
}

graphicsSpriteItem :: MonadIO m => SDL.Renderer -> FilePath -> m GraphicsItem
graphicsSpriteItem renderer texturePath = do
  s <- Just . RenderSprite <$> sprite renderer texturePath
  return $ set itemRenderItem s graphicsItem

graphicsTextItem :: MonadIO m => SDL.Renderer -> TTF.Font ->
                                 SDL.V4 Word8 -> String -> m GraphicsItem
graphicsTextItem renderer font color text = do
  s <- Just . RenderSprite <$> textSprite renderer font color text
  return $ set itemRenderItem s graphicsItem

sprite :: MonadIO m => SDL.Renderer -> FilePath -> m Sprite
sprite renderer texturePath = do
  (texture, w, h) <- loadTexture renderer texturePath
  return $ Sprite texture AnchorCenter w h Nothing

textSprite :: MonadIO m => SDL.Renderer -> TTF.Font -> SDL.V4 Word8 -> String -> m Sprite
textSprite renderer font color text = do
  surface <- TTF.blended font color (Text.pack text)
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
  SDL.updateTexture tex Nothing (byteStringFromVector (I.imageData image)) (4 * w)
  SDL.textureBlendMode tex SDL.$= SDL.BlendAlphaBlend
  return (tex, fromIntegral w, fromIntegral h)

renderSprite :: MonadIO m => Sprite -> RenderContext m ()
renderSprite sprite = do
  (x, y) <- askOrigin
  r <- asks renderer
  let (xp, yp) = spriteTopLeft x y sprite
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

renderLine :: MonadIO m => LineInfo -> RenderContext m ()
renderLine (LineInfo (dx, dy) color) = do
  (x, y) <- askOrigin
  r <- asks renderer
  let xi1 = fromIntegral $ round x
      yi1 = fromIntegral $ round y
      xi2 = fromIntegral $ round (x + dx)
      yi2 = fromIntegral $ round (y + dy)
  SDL.rendererDrawColor r SDL.$= color
  SDL.drawLine r (SDL.P (SDL.V2 xi1 yi1)) (SDL.P (SDL.V2 xi2 yi2))

renderItem :: MonadIO m => GraphicsItem -> RenderContext m ()
renderItem item =
  when (view itemVisible item) $ do
    (ox, oy) <- askOrigin
    let x = ox + view itemX item
        y = oy + view itemY item
        renderActualItem (RenderSprite sprite) = renderSprite sprite
        renderActualItem (RenderLine line)     = renderLine line
    local (set originX x . set originY y) $
      mapM_ renderActualItem (view itemRenderItem item)

render :: (MonadIO m, Scene s) => SDL.Renderer -> s -> m ()
render r scene = do
  SDL.rendererDrawColor r SDL.$= clearColor scene
  SDL.clear r
  traverseItems_ (\i -> runReaderT (renderItem i) (renderEnv r)) scene
  SDL.present r

renderLoop :: (W.HasTime t s, Monoid e, MonadIO m, Scene sc) =>
              SDL.Renderer -> sc -> W.Session m s -> W.Wire s e m (sc, [SDL.Event]) sc -> m ()
renderLoop renderer scene session wire = do
  events <- SDL.pollEvents
  (step, session') <- W.stepSession session
  (output, wire') <- W.stepWire wire step $ Right (scene, events)
  either (const (pure ())) (\scene' -> do
      render renderer scene'
      renderLoop renderer scene' session' wire')
    output
