module StreetRun.GameOver where

import Control.Monad (when)
import Control.Monad.State (MonadState, gets, modify)
import Control.Lens ((%~), view)
import KeyState

import StreetRun.Renderer
import StreetRun.Renderer_func_1
import StreetRun.Renderer_func_2

import StreetRun.Picture
import StreetRun.HUD
import StreetRun.Input
import StreetRun.Common
import StreetRun.Play
import StreetRun.Work
import StreetRun.Work_func
import StreetRun.Out
import StreetRun.Out_func
import StreetRun.Load
import StreetRun.Scene

class Monad m => GameOver m where
  gameOverStep :: m ()

gameOverStep' :: (HasPlayVars s, HasCommonVars s, HasGameOverVars s, MonadState s m, SceneManager m, HasInput m, Renderer m, CameraControl m, HUD m) => m ()
gameOverStep' = do
  input <- getInput
  updateGameOver
  drawPlay
  drawGameOver
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Title)

updateGameOver :: (HasGameOverVars s, HasPlayVars s, MonadState s m) => m ()
updateGameOver = modify $ gameOverVars %~ stepGameOverVars

drawGameOver :: (Renderer m, CameraControl m, MonadState s m, HasGameOverVars s) => m ()
drawGameOver = do
  gov <- gets (view gameOverVars)
  drawBlackOverlay (govFadeout gov)
  drawGameOverText (470,300)
  when (gameOverShowPressSpace $ govSpaceFlashing gov) $ drawPressSpaceText (550,500)
