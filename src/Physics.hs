{-# LANGUAGE RankNTypes #-}

module Physics where

import Graphics

import Control.Arrow
import Control.Lens

frame :: Lens' m Float -> Lens' m Float -> Lens' m (Float, Float)
frame pos vel = lens (view pos &&& view vel)
                (\c (p, v) -> (set pos p . set vel v) c)

class Located m => Moving m where
  xVel :: Lens' m Float
  yVel :: Lens' m Float
  xFrame :: Lens' m (Float, Float)
  xFrame = frame xPos xVel
  yFrame :: Lens' m (Float, Float)
  yFrame = frame yPos yVel
