{-# LANGUAGE TemplateHaskell #-}

module Scene where

import Graphics
import Physics

import Paths_beacHball

import Control.Arrow
import Control.Lens
import Data.Maybe
import qualified SDL
import System.Random

sceneGravity = 2500

-- Circle with radius = width and located at top of sprite
collisionCircle :: GraphicsItem -> CollisionShape
collisionCircle item =
  case view itemRenderItem item of
    RenderSprite s ->
      let r = fromIntegral (view w s) / 2
          (x, y) = (view itemX item, view itemY item)
          (sx, sy) = spriteTopLeft x y s
      in CollisionCircle ((sx + r, sy + r), r)
    _ -> CollisionNone

data Player = Player {
  _leftKey :: SDL.Scancode,
  _rightKey :: SDL.Scancode,
  _upKey :: SDL.Scancode,
  _playerHBounds :: (Float, Float),
  _playerObject :: Object
}
makeLenses ''Player

instance Located Player where
  xPos = playerObject . xPos
  yPos = playerObject . yPos

instance Moving Player where
  xVel = playerObject . xVel
  yVel = playerObject . yVel
  gravity = gravity . view playerObject
  collisionShape = collisionShape . view playerObject

data Ball = Ball {
  _ballRandomGen :: StdGen,
  _ballAV :: Float,
  _ballObject :: Object
}
makeLenses ''Ball

instance Located Ball where
  xPos = ballObject . xPos
  yPos = ballObject . yPos

instance Moving Ball where
  xVel = ballObject . xVel
  yVel = ballObject . yVel
  gravity = gravity . view ballObject
  collisionShape = collisionShape . view ballObject

unsafeSprite :: Lens' GraphicsItem Sprite
unsafeSprite = lens unsafeGetSprite (\item s -> set itemRenderItem (RenderSprite s) item)
  where
    unsafeGetSprite item = case view itemRenderItem item of
      RenderSprite s -> s
      _ -> undefined


-- ball must have sprite and transformation
ballA :: Lens' Ball Float
ballA = lens (view transformAngle . fromJust . view (ballObject . objItem . unsafeSprite . spriteTransform))
             (\b a -> set (ballObject . objItem . unsafeSprite . spriteTransform)
                          (Just (set transformAngle a (fromJust $ view (ballObject . objItem . unsafeSprite . spriteTransform) b)))
                          b)

ballAFrame :: Lens' Ball (Float, Float)
ballAFrame = frame ballA ballAV

ballRandomR :: Random a => (a, a) -> Ball -> (a, Ball)
ballRandomR range b =
  let (a, g) = randomR range (view ballRandomGen b)
  in  (a, set ballRandomGen g b)

data GameScene = GameScene {
  _width :: Float,
  _height :: Float,
  _baseY :: Float,
  _ground :: Object,
  _leftWall :: Object,
  _rightWall :: Object,
  _sun :: GraphicsItem,
  _clouds :: [Object],
  _background :: GraphicsItem,
  _ball :: Ball,
  _player1 :: Player,
  _player2 :: Player
}
makeLenses ''GameScene

instance Scene GameScene where
  forItems_ s f =
    f (view sun s) *>
    forOf_ (clouds . traverse . objItem) s f *>
    f (view background s) *>
    f (view (ball . ballObject . objItem) s) *>
    f (view (player1 . playerObject . objItem) s) *>
    f (view (player2 . playerObject . objItem) s) *>
    pure ()
  clearColor _ = SDL.V4 155 220 255 255

createPlayer :: SDL.Renderer -> Float -> SDL.Scancode -> SDL.Scancode -> SDL.Scancode
                -> (Float -> Float) -> (Float -> Float) -> IO Player
createPlayer r base left right up getMinX getMaxX = do
  item <- graphicsSpriteItem r =<< getDataFileName "potato_sml.png"
  let halfSpriteW = fromIntegral (view (unsafeSprite . w) item) / 2
      minX = getMinX halfSpriteW
      maxX = getMaxX halfSpriteW
      player = Player left right up (minX, maxX) object
  return $ set xPos ((minX + maxX) / 2) .
           set yPos base .
           set (playerObject . objGravity) sceneGravity .
           set (playerObject . objCollisionShape) (collisionCircle . view objItem) .
           set (playerObject . objItem . unsafeSprite . anchor) AnchorBottomMid .
           set (playerObject . objItem) item
           $ player

createPlayer1 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer1 r width base =
  createPlayer r base SDL.ScancodeA SDL.ScancodeD SDL.ScancodeW getMinX getMaxX
  where getMinX = id
        getMaxX halfSpriteW = width / 2 - halfSpriteW

createPlayer2 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer2 r width base =
  set (playerObject . objItem . unsafeSprite . spriteTransform) (Just $ SpriteTransform 0 (True, False))
    <$> createPlayer r base SDL.ScancodeLeft SDL.ScancodeRight SDL.ScancodeUp getMinX getMaxX
  where getMinX halfSpriteW = width / 2 + halfSpriteW
        getMaxX halfSpriteW = width - halfSpriteW

setRandomAV :: Ball -> Ball
setRandomAV b =
  let (av, b') = ballRandomR (50, 300) b
      av' = if view xVel b' < 0 then -av else av
  in  set ballAV av' b'

createBall :: SDL.Renderer -> Float -> Float -> IO Ball
createBall r width height = do
  item <- graphicsSpriteItem r =<< getDataFileName "ball.png"
  rgen <- newStdGen
  xv <- randomRIO (-40, 40)
  let ball = Ball rgen 0 object
  return $ set xPos (width - width / 3) .
           set yPos (height / 3) .
           set xVel (xv * 20) .
           set (ballObject . objGravity) (sceneGravity / 2) .
           set (ballObject . objItem . unsafeSprite . spriteTransform) (Just $ SpriteTransform 0 (False, False)) .
           set (ballObject . objCollisionShape) (collisionCircle . view objItem) .
           set (ballObject . objItem) item
           $ setRandomAV ball

createSun :: SDL.Renderer -> Float -> IO GraphicsItem
createSun r w = do
  item <- graphicsSpriteItem r =<< getDataFileName "sun.png"
  return $ set xPos (3 * w / 4) . set yPos 0 . set (unsafeSprite . anchor) AnchorTopMid $ item

createBackground :: SDL.Renderer -> Float -> Float -> IO GraphicsItem
createBackground r w h = do
  (tex, twi, thi) <- loadTexture r =<< getDataFileName "background.png"
  let xp = w / 2
      yp = h
      tw = fromIntegral twi :: Float
      th = fromIntegral thi :: Float
      (sw, sh) = (round w, round (th * w / tw))
      sprite = Sprite tex AnchorBottomMid sw sh Nothing
  return $ GraphicsItem xp yp (RenderSprite sprite)

createCloud :: SDL.Renderer -> Float -> Float -> FilePath -> IO Object
createCloud r w h path = do
  item <- graphicsSpriteItem r path
  xp <- randomRIO (- w / 2, w)
  yp <- randomRIO (0, h / 4)
  v <- randomRIO (2, 40)
  return $ set (objItem . unsafeSprite . anchor) AnchorTopLeft .
           set xPos xp .
           set yPos yp .
           set xVel v .
           set objItem item
           $ object

createClouds :: SDL.Renderer -> Float -> Float -> IO [Object]
createClouds r w h = do
  paths <- mapM getDataFileName ["cloud" ++ show n ++ ".png" | n <- [1..5]]
  mapM (createCloud r w h) paths

startScene :: SDL.Window -> SDL.Renderer -> IO GameScene
startScene window renderer = do
  windowConfig <- SDL.getWindowConfig window
  let (SDL.V2 wi hi) = SDL.windowInitialSize windowConfig
      width = fromIntegral wi
      height = fromIntegral hi
      base = height - height / 7
      ground = set yPos base .
               set objCollisionShape (const $ CollisionLine ((0, base), (0, -1)))
               $ object
      leftWall = set objCollisionShape (const $ CollisionLine ((0, 0), (1, 0))) object
      rightWall = set xPos width .
                  set objCollisionShape (const $ CollisionLine ((width, 0), (-1, 0)))
                  $ object
  p1 <- createPlayer1 renderer width base
  p2 <- createPlayer2 renderer width base
  sun <- createSun renderer width
  bg <- createBackground renderer width height
  clouds <- createClouds renderer width height
  ball <- createBall renderer width height
  return $ GameScene width height base ground leftWall rightWall sun clouds bg ball p1 p2
