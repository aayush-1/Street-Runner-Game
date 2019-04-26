module StreetRun.Death where

import qualified Animate
import Control.Monad (when)
import Control.Lens (view)
import Control.Monad.State (MonadState, gets)

import StreetRun.Renderer
import StreetRun.Renderer_func_1
import StreetRun.Renderer_func_2

import StreetRun.HUD
import StreetRun.Picture
import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.CFL
import StreetRun.Common
import StreetRun.Work
import StreetRun.Work_func
import StreetRun.Play
import StreetRun.Load
import StreetRun.Scene

class Monad m => Death m where
  deathStep :: m ()

deathStep' :: (HasPlayVars s, HasCommonVars s, MonadState s m, SceneManager m, HasInput m, Renderer m, CameraControl m, HUD m) => m ()
deathStep' = do
  updateDeath
  drawPlay
  height <- gets (dsHeight . pvRunnerState . view playVars)
  case height of
    Nothing -> return ()
    Just h -> when (h > pi / 2) (toScene Scene'GameOver)

updateDeath :: (HasPlayVars s, MonadState s m, Renderer m) => m ()
updateDeath = do
  animations <- getRunnerAnimations
  modifyPlayVars $ \pv -> let
    ds = pvRunnerState pv
    ds' = ds { dsHeight = maybe (Just 0) (\x -> Just $ x + 0.03) (dsHeight ds) }
    in pv { pvRunnerState = ds', pvRunnerPos = Animate.stepPosition animations (pvRunnerPos pv) frameDeltaSeconds }
