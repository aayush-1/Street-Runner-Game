module StreetRun.SDLLayer where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..)) --Mondas with Embedded IO operations 
import Foreign.C.Types --mapping of C types to haskell types 
import SDL.Vect

class Monad m => SDLRenderer m where
  presentRenderer :: SDL.Renderer -> m ()
  clearRenderer :: SDL.Renderer -> m ()
  queryTexture :: SDL.Texture -> m SDL.TextureInfo 
  drawTexture :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt)-> Maybe (SDL.Rectangle CInt) -> m ()

updateWindowSurface' :: MonadIO m => SDL.Window -> m ()
updateWindowSurface' window = liftIO $ SDL.updateWindowSurface window
--This is used to reflect any changes to the surface on the screen. 

presentRenderer' :: MonadIO m => SDL.Renderer -> m ()
presentRenderer' = SDL.present

clearSurface' :: MonadIO m => SDL.Surface -> m ()
clearSurface' screen = liftIO $ SDL.surfaceFillRect screen Nothing (V4 0 0 0 0) 
-- screen : drawing target
-- Nothing to fill the whole surface 
-- V4 is color to fill with 

drawTexture' :: MonadIO m => SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> m ()
drawTexture' renderer tex maybeClip maybeLoc = SDL.copy renderer tex maybeClip maybeLoc
-- rendering texture from source rectangle to destination rectangle  (copy a potion to destination)

clearRenderer' :: MonadIO m => SDL.Renderer -> m ()
clearRenderer' = SDL.clear -- clears the current rendering target

queryTexture' ::  MonadIO m => SDL.Texture -> m SDL.TextureInfo
queryTexture' = SDL.queryTexture --get text info (px format,access,width and height of texture)

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