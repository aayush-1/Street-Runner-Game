module StreetRun.Scene.GameOver where

import Control.Monad (when)
import Control.Monad.State (MonadState, gets, modify)
import Control.Lens ((%~), view)
import KeyState

import StreetRun.Effect.Renderer
import StreetRun.Effect.Camera
import StreetRun.Effect.HUD
import StreetRun.Engine.Input
import StreetRun.Engine.Common
import StreetRun.Scene.Play
import StreetRun.Engine.Play
import StreetRun.Engine.GameOver
import StreetRun.Manager.Input
import StreetRun.Manager.Scene

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
  disableZoom
  drawGameOverText (470,300)
  when (gameOverShowPressSpace $ govSpaceFlashing gov) $ drawPressSpaceText (550,500)
  enableZoom
