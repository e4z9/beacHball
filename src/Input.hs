module Input where

import qualified Control.Monad as M
import Control.Wire
import Data.Foldable (foldl')
import Data.Maybe
import qualified Data.Set as Set
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

untilQuitOrClose :: (Monoid e, Monad m) => Wire s e m [SDL.Event] ()
untilQuitOrClose = until <<< pure () &&& closeOrQuitRequestedEv

type Keys = Set.Set SDL.Keysym

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
