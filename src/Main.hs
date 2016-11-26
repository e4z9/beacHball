{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

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

playerSpeed :: Float
playerSpeed = 200

speedControl :: (Monoid e, Monad m) => SDL.Scancode -> SDL.Scancode -> Wire s e m Keys Float
speedControl left right = pure 0 . scancodePressed left . scancodePressed right
      <|> pure (-playerSpeed) . scancodePressed left
      <|> pure playerSpeed . scancodePressed right
      <|> pure 0

pos1 :: (HasTime t s, Monoid e, Monad m) => Wire s e m Keys Float
pos1 = integral 50 . speedControl SDL.ScancodeA SDL.ScancodeD

pos2 :: (HasTime t s, Monoid e, Monad m) => Wire s e m Keys Float
pos2 = integral 350 . speedControl SDL.ScancodeLeft SDL.ScancodeRight

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

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "BeacHball" SDL.defaultWindow
  let vsyncRenderer = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  renderer <- SDL.createRenderer window anyRenderingDriver vsyncRenderer
  scene <- startScene renderer
  renderLoop renderer scene clockSession_ logic
  SDL.quit
