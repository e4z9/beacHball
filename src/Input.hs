{-# LANGUAGE TemplateHaskell #-}

module Input where

import qualified Control.Monad as M
import Control.Lens
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

data Keys = Keys {
  _pressedKeys :: Set.Set SDL.Keysym,
  _currentKeys :: Set.Set SDL.Keysym
}
makeLenses ''Keys

emptyKeys = Keys mempty mempty
reinitKeys = set pressedKeys mempty

keyboardEventData :: SDL.Event -> Maybe SDL.KeyboardEventData
keyboardEventData e =
  case SDL.eventPayload e of
    SDL.KeyboardEvent d -> Just d
    _ -> Nothing

updateKeys :: [SDL.Event] -> Keys -> Keys
updateKeys es ks = foldl' updateFromKeyData (reinitKeys ks) keyEvents
  where
    keyEvents = filter (not . SDL.keyboardEventRepeat) $ mapMaybe keyboardEventData es
    updateFromKeyData keys d =
      case SDL.keyboardEventKeyMotion d of
        SDL.Pressed  -> over currentKeys (Set.insert (SDL.keyboardEventKeysym d)) .
                        over pressedKeys (Set.insert (SDL.keyboardEventKeysym d)) $ keys
        SDL.Released -> over currentKeys (Set.delete (SDL.keyboardEventKeysym d)) keys

handleKeyEvents :: Wire s e m [SDL.Event] Keys
handleKeyEvents = handleKeyEvents' emptyKeys
  where handleKeyEvents' keys = mkPureN $ \es ->
          let newKeys = updateKeys es keys
          in (Right newKeys, handleKeyEvents' newKeys) -- does this need to be made stricter in newKeys?

isScancodeDown :: SDL.Scancode -> Keys -> Bool
isScancodeDown code = any ((==) code . SDL.keysymScancode) . view currentKeys

isScancodePressed :: SDL.Scancode -> Keys -> Bool
isScancodePressed code = any ((==) code . SDL.keysymScancode) . view pressedKeys
