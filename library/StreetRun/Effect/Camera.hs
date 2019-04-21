module StreetRun.Effect.Camera where

import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, modify, gets)
import Data.StateVar (($=))
import SDL.Vect

import StreetRun.Config
import StreetRun.State
import StreetRun.Engine.Camera 

class Monad m => CameraControl m where --m() indicates that output is monadic 
  adjustCamera :: Camera -> m ()
  disableZoom :: m ()
  enableZoom :: m ()

adjustCamera' :: (MonadIO m, MonadReader Config m, MonadState Vars m) => Camera -> m ()
adjustCamera' cam = do
  modify ( \v -> v { vCamera = cam } )--setting vcam variable from vars to cam
  renderer <- asks cRenderer --returns it's value to renderer (tied to SDL_window)
  moveCamera renderer cam 

disableZoom' :: (MonadIO m, MonadReader Config m) => m ()-- disables zoom by calling initial settings on camera
disableZoom' = do
  renderer <- asks cRenderer
  moveCamera renderer initCamera

enableZoom' :: (MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
enableZoom' = do -- enables zoom
  renderer <- asks cRenderer
  cam <- gets vCamera
  moveCamera renderer cam

moveCamera :: MonadIO m => SDL.Renderer -> Camera -> m ()
moveCamera renderer Camera{camZoom, camOrigin} = do
  SDL.rendererScale renderer $= (fmap realToFrac camZoom) --set the drawing scale(fractional type ) for rendering on current target(pointed to by renderer)
  let dim = fmap truncate $ screenV2 -- truncate x : returns the integer nearest x between zero and x
  SDL.rendererViewport renderer $= (Just $ SDL.Rectangle (SDL.P $ (fmap truncate $ moveOrigin camOrigin)) dim)
  SDL.rendererClipRect renderer $= (Just $ SDL.Rectangle (SDL.P $ V2 0 0) dim)
