module StreetRun.Pause where

import Control.Monad (when)
import Control.Monad.State (MonadState)
import KeyState

import StreetRun.Renderer
import StreetRun.Renderer_func_1
import StreetRun.Renderer_func_2

import StreetRun.HUD
import StreetRun.Picture
import StreetRun.Input
import StreetRun.Common
import StreetRun.Work
import StreetRun.Work_func
import StreetRun.Play
import StreetRun.Load
import StreetRun.Scene

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
  drawPauseText (530,270)
