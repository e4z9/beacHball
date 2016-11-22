{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import qualified Control.Monad as M
import Control.Monad.IO.Class
import Control.Wire
import Data.Foldable (foldl')
import Data.Int (Int32)
import Data.Maybe
import qualified Data.Set as Set
import Foreign.C.Types
import FRP.Netwire
import qualified SDL
import Prelude hiding ((.), until)

isCloseOrQuitEvent :: SDL.Event -> Bool
isCloseOrQuitEvent event =
  case SDL.eventPayload event of
    SDL.WindowClosedEvent _ -> True
    SDL.QuitEvent -> True
    _ -> False

closeOrQuitRequestedEv :: Monad m => Wire s e m [SDL.Event] (Event ())
closeOrQuitRequestedEv = M.void <$> became (any isCloseOrQuitEvent)

type Keys = Set.Set SDL.Keysym
noKeys :: Keys
noKeys = mempty

keyboardEventData :: SDL.Event -> Maybe SDL.KeyboardEventData
keyboardEventData e =
  case SDL.eventPayload e of
    SDL.KeyboardEvent d -> Just d
    _ -> Nothing

updateKeys :: [SDL.Event] -> Keys -> Keys
updateKeys es ks = foldl' updateFromKeyData ks $ mapMaybe keyboardEventData es
  where updateFromKeyData keys d =
          case SDL.keyboardEventKeyMotion d of
            SDL.Pressed -> Set.insert (SDL.keyboardEventKeysym d) keys
            SDL.Released -> Set.delete (SDL.keyboardEventKeysym d) keys

handleKeyEvents :: Wire s e m [SDL.Event] Keys
handleKeyEvents = handleKeyEvents' mempty
  where handleKeyEvents' keys = mkPureN $ \es ->
          let newKeys = updateKeys es keys
          in (Right newKeys, handleKeyEvents' newKeys) -- does this need to be made stricter in newKeys?

isScancodePressed :: SDL.Scancode -> Keys -> Bool
isScancodePressed code = any ((==) code . SDL.keysymScancode)

scancodePressed :: (Monoid e, Monad m) => SDL.Scancode -> Wire s e m Keys Keys
scancodePressed k = when (isScancodePressed k)

dx :: RealFrac n => n
dx = 200

speed :: (Monoid e, Monad m, RealFrac n) => Wire s e m Keys n
speed  =  pure 0 . scancodePressed SDL.ScancodeA . scancodePressed SDL.ScancodeD
      <|> pure (-dx) . scancodePressed SDL.ScancodeA
      <|> pure dx . scancodePressed SDL.ScancodeD
      <|> pure 0

pos :: (HasTime t s, Monoid e, Monad m, RealFrac n) => Wire s e m Keys n
pos = integral 0 . speed

untilQuitOrClose :: (Monoid e, Monad m) => Wire s e m [SDL.Event] ()
untilQuitOrClose = until <<< pure () &&& closeOrQuitRequestedEv

logic :: (HasTime t s, Monad m, RealFrac n) => Wire s () m [SDL.Event] n
logic = proc events -> do
  untilQuitOrClose -< events
  keys <- handleKeyEvents -< events
  pos -< keys

anyRenderingDriver = -1

render :: (RealFrac n, MonadIO m) => n -> SDL.Renderer-> m ()
render x r = do
  let xp = fromInteger $ toInteger $ round x
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 255
  SDL.clear r
  SDL.rendererDrawColor r SDL.$= SDL.V4 255 255 255 255
  SDL.fillRect r $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (CInt xp) 200)) (SDL.V2 40 40)
  SDL.present r

renderLoop :: (HasTime t s, Monoid e, MonadIO m, RealFrac n) => SDL.Renderer -> Session m s -> Wire s e m [SDL.Event] n -> m ()
renderLoop renderer session wire = do
  events <- SDL.pollEvents
  (step, session') <- stepSession session
  (output, wire') <- stepWire wire step $ Right events
  either (const (pure ())) (\x -> do
      render x renderer
      renderLoop renderer session' wire')
    output

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Netwire Test" SDL.defaultWindow
  let vsyncRenderer = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  renderer <- SDL.createRenderer window anyRenderingDriver vsyncRenderer
  renderLoop renderer clockSession_ logic
  SDL.quit
