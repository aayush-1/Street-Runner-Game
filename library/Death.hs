module StreetRun.Scene.Death where

import qualified Animate
import Control.Monad (when)
import Control.Lens (view)
import Control.Monad.State (MonadState, gets)

import StreetRun.Effect.Renderer
import StreetRun.Effect.HUD
import StreetRun.Effect.Camera
import StreetRun.Engine.Runner
import StreetRun.Engine.Frame
import StreetRun.Engine.Common
import StreetRun.Scene.Play
import StreetRun.Engine.Play
import StreetRun.Manager.Input
import StreetRun.Manager.Scene

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
