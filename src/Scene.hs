{-# LANGUAGE TemplateHaskell #-}

module Scene where

import Graphics

import Paths_beacHball

import Control.Lens
import qualified SDL
import System.Random

data Player = Player {
  _leftKey :: SDL.Scancode,
  _rightKey :: SDL.Scancode,
  _upKey :: SDL.Scancode,
  _playerHBounds :: (Float, Float),
  _playerYV :: Float,
  _playerSprite :: Sprite
}
makeLenses ''Player

playerX :: Lens' Player Float
playerX = playerSprite . x

playerY :: Lens' Player Float
playerY = playerSprite . y

data Cloud = Cloud {
  _cloudXV :: Float,
  _cloudSprite :: Sprite
}
makeLenses ''Cloud

cloudX :: Lens' Cloud Float
cloudX = cloudSprite . x

data GameScene = GameScene {
  _width :: Float,
  _height :: Float,
  _sun :: Sprite,
  _clouds :: [Cloud],
  _background :: Sprite,
  _player1 :: Player,
  _player2 :: Player
}
makeLenses ''GameScene

instance Scene GameScene where
  renderScene s f = do
    f $ view sun s
    mapM_ (f . view cloudSprite) $ view clouds s
    f $ view background s
    f $ view (player1 . playerSprite) s
    f $ view (player2 . playerSprite) s
  clearColor _ = SDL.V4 155 220 255 255

createPlayer1 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer1 r width height = do
  sprite <- createSprite r =<< getDataFileName "potato_sml.png"
  let halfSpriteW = fromIntegral (view w sprite) / 2
      minX = halfSpriteW
      maxX = width / 2 - halfSpriteW
      player = Player SDL.ScancodeA SDL.ScancodeD SDL.ScancodeW
                      (minX, maxX) 0 sprite
  return $ set playerX (width / 4) .
           set playerY (height - height / 5)
           $ player

createPlayer2 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer2 r width height = do
  sprite <- createSprite r =<< getDataFileName "potato_sml2.png"
  let halfSpriteW = fromIntegral (view w sprite) / 2
      minX = width / 2 + halfSpriteW
      maxX = width - halfSpriteW
      player = Player SDL.ScancodeLeft SDL.ScancodeRight SDL.ScancodeUp
                      (minX, maxX) 0 sprite
  return $ set playerX (width - width / 4) .
           set playerY (height - height / 5)
           $ player

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
  return $ Sprite tex AnchorBottomMid xp yp sw sh

createCloud :: SDL.Renderer -> Float -> Float -> FilePath -> IO Cloud
createCloud r w h path = do
  sprite <- createSprite r path
  xp <- randomRIO (- w / 2, w)
  yp <- randomRIO (0, h / 4)
  v <- randomRIO (2, 40)
  return $ Cloud v $ set x xp . set y yp . set anchor AnchorTopLeft $ sprite

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
  p1 <- createPlayer1 renderer width height
  p2 <- createPlayer2 renderer width height
  sun <- createSun renderer width
  bg <- createBackground renderer width height
  clouds <- createClouds renderer width height
  return $ GameScene width height sun clouds bg p1 p2
