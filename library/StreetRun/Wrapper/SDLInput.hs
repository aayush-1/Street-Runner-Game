module StreetRun.Wrapper.SDLInput where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))

keycodePressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodePressed keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == keycode &&
    motion == SDL.Pressed &&
    not keyboardEventRepeat
  _ -> False

keycodeReleased :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodeReleased keycode event = case event of --Keyboard Event data means that if any key has been pressed or released or held 
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == keycode &&
    motion == SDL.Released &&  --input motion (released or pressed)
    not keyboardEventRepeat
  _ -> False

class Monad m => SDLInput m where
  pollEventPayloads :: m [SDL.EventPayload]

pollEventPayloads' :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads' = liftIO $ map SDL.eventPayload <$> SDL.pollEvents
