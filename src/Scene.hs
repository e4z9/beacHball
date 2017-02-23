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

data Player = Player {
  _leftKey :: SDL.Scancode,
  _rightKey :: SDL.Scancode,
  _upKey :: SDL.Scancode,
  _playerHBounds :: (Float, Float),
  _playerXV :: Float,
  _playerYV :: Float,
  _playerSprite :: Sprite
}
makeLenses ''Player

instance Located Player where
  xPos = playerSprite . x
  yPos = playerSprite . y

instance Moving Player where
  xVel = playerXV
  yVel = playerYV
  gravity = const sceneGravity

-- Circle with radius = width and located at top of sprite
collisionCircle :: Sprite -> ((Float, Float), Float) -- ((x, y), r)
collisionCircle s =
  let r = fromIntegral (view w s) / 2
      (sx, sy) = spriteTopLeft s
  in ((sx + r, sy + r), r)

data Cloud = Cloud {
  _cloudXV :: Float,
  _cloudYV :: Float,
  _cloudSprite :: Sprite
}
makeLenses ''Cloud

instance Located Cloud where
  xPos = cloudSprite . x
  yPos = cloudSprite . y

instance Moving Cloud where
  xVel = cloudXV
  yVel = cloudYV

data Ball = Ball {
  _ballRandomGen :: StdGen,
  _ballAV :: Float,
  _ballXV :: Float,
  _ballYV :: Float,
  _ballSprite :: Sprite
}
makeLenses ''Ball

instance Located Ball where
  xPos = ballSprite . x
  yPos = ballSprite . y

instance Moving Ball where
  xVel = ballXV
  yVel = ballYV
  gravity = const (sceneGravity / 2)

-- ball must have transformation
ballA :: Lens' Ball Float
ballA = lens (view transformAngle . fromJust . view (ballSprite . spriteTransform))
             (\b a -> set (ballSprite . spriteTransform)
                          (Just (set transformAngle a (fromJust $ view (ballSprite . spriteTransform) b)))
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
  _sun :: Sprite,
  _clouds :: [Cloud],
  _background :: Sprite,
  _ball :: Ball,
  _player1 :: Player,
  _player2 :: Player
}
makeLenses ''GameScene

instance Scene GameScene where
  renderScene s f = do
    f $ view sun s
    mapM_ (f . view cloudSprite) $ view clouds s
    f $ view background s
    f $ view (ball . ballSprite) s
    f $ view (player1 . playerSprite) s
    f $ view (player2 . playerSprite) s
  clearColor _ = SDL.V4 155 220 255 255

createPlayer1 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer1 r width base = do
  sprite <- createSprite r =<< getDataFileName "potato_sml.png"
  let halfSpriteW = fromIntegral (view w sprite) / 2
      minX = halfSpriteW
      maxX = width / 2 - halfSpriteW
      player = Player SDL.ScancodeA SDL.ScancodeD SDL.ScancodeW
                      (minX, maxX) 0 0 sprite
  return $ set xPos (width / 4) .
           set yPos base .
           set (playerSprite . anchor) AnchorBottomMid
           $ player

createPlayer2 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer2 r width base = do
  sprite <- createSprite r =<< getDataFileName "potato_sml.png"
  let halfSpriteW = fromIntegral (view w sprite) / 2
      minX = width / 2 + halfSpriteW
      maxX = width - halfSpriteW
      player = Player SDL.ScancodeLeft SDL.ScancodeRight SDL.ScancodeUp
                      (minX, maxX) 0 0 sprite
  return $ set xPos (width - width / 4) .
           set yPos base .
           set (playerSprite . anchor) AnchorBottomMid .
           set (playerSprite . spriteTransform) (Just $ SpriteTransform 0 (True, False))
           $ player

setRandomAV :: Ball -> Ball
setRandomAV b =
  let (av, b') = ballRandomR (50, 300) b
      av' = if view xVel b' < 0 then -av else av
  in  set ballAV av' b'

createBall :: SDL.Renderer -> Float -> Float -> IO Ball
createBall r width height = do
  sprite <- createSprite r =<< getDataFileName "ball.png"
  rgen <- newStdGen
  xv <- randomRIO (-40, 40)
  let ball = Ball rgen 0 (xv * 20) 0 sprite
  return $ set xPos (width - width / 3) .
           set yPos (height / 3) .
           set (ballSprite . spriteTransform) (Just $ SpriteTransform 0 (False, False))
           $ setRandomAV ball

createSun :: SDL.Renderer -> Float -> IO Sprite
createSun r w = do
  sprite <- createSprite r =<< getDataFileName "sun.png"
  return $ set x (3 * w / 4) . set y 0 . set anchor AnchorTopMid $ sprite

createBackground :: SDL.Renderer -> Float -> Float -> IO Sprite
createBackground r w h = do
  (tex, twi, thi) <- loadTexture r =<< getDataFileName "background.png"
  let xp = w / 2
      yp = h
      tw = fromIntegral twi :: Float
      th = fromIntegral thi :: Float
      (sw, sh) = (round w, round (th * w / tw))
  return $ Sprite tex AnchorBottomMid xp yp sw sh Nothing

createCloud :: SDL.Renderer -> Float -> Float -> FilePath -> IO Cloud
createCloud r w h path = do
  sprite <- createSprite r path
  xp <- randomRIO (- w / 2, w)
  yp <- randomRIO (0, h / 4)
  v <- randomRIO (2, 40)
  return $ Cloud v 0 $ set x xp . set y yp . set anchor AnchorTopLeft $ sprite

createClouds :: SDL.Renderer -> Float -> Float -> IO [Cloud]
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
  p1 <- createPlayer1 renderer width base
  p2 <- createPlayer2 renderer width base
  sun <- createSun renderer width
  bg <- createBackground renderer width height
  clouds <- createClouds renderer width height
  ball <- createBall renderer width height
  return $ GameScene width height base sun clouds bg ball p1 p2
