module StreetRun.Scene.Pause where

import Control.Monad (when)
import Control.Monad.State (MonadState)
import KeyState

import StreetRun.Effect.Renderer
import StreetRun.Effect.HUD
import StreetRun.Effect.Camera
import StreetRun.Engine.Input
import StreetRun.Engine.Common
import StreetRun.Engine.Play
import StreetRun.Scene.Play
import StreetRun.Manager.Input
import StreetRun.Manager.Scene

class Monad m => Pause m where
  pauseStep :: m ()

pauseStep' :: (HasPlayVars s, HasCommonVars s, MonadState s m, SceneManager m, HasInput m, Renderer m, CameraControl m, HUD m) => m ()
pauseStep' = do
  input <- getInput
  drawPlay
  drawPause
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)

drawPause :: (Renderer m, CameraControl m) => m ()
drawPause = do
  drawBlackOverlay 0.8
  disableZoom
  drawPauseText (530,270)
  enableZoom
