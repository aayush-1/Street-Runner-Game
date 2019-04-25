{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Title where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState

import StreetRun.Config
import StreetRun.Renderer
import StreetRun.Renderer_func_1
import StreetRun.Renderer_func_2

import StreetRun.HUD
import StreetRun.Sound_effects
import StreetRun.Input
import StreetRun.Frame
import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.Tag
import StreetRun.Tag_func
import StreetRun.Common
import StreetRun.Quake
import StreetRun.Quake_Building
import StreetRun.Quake_BackStreet
import StreetRun.Quake_Street
import StreetRun.Quake_Grass
import StreetRun.Load
import StreetRun.Scene

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (HasTitleVars s, HasCommonVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, HUD m, AudioSfx m) => m ()
titleStep' = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Play)
  updateTitle
  drawTitle

updateTitle :: (HasTitleVars s, HasCommonVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, AudioSfx m) => m ()
updateTitle = do
  runnerAnimations <- getRunnerAnimations
  runnerPos <- gets (tvRunnerPos . view titleVars)
  let runnerPos' = Animate.stepPosition runnerAnimations runnerPos frameDeltaSeconds

  buildingAnimations <- getBuildingAnimations
  buildingPos <- gets (tvBuildingPos . view titleVars)
  let buildingPos' = Animate.stepPosition buildingAnimations buildingPos frameDeltaSeconds

  grassAnimations <- getGrassAnimations
  grassPos <- gets (tvGrassPos . view titleVars)
  let grassPos' = Animate.stepPosition grassAnimations grassPos frameDeltaSeconds

  modify $ titleVars %~ (\tv -> tv
    { tvRunnerPos = runnerPos'
    , tvBuildingPos = buildingPos'
    , tvGrassPos = grassPos'
    , tvFlashing = tvFlashing tv + 0.025
    })

drawTitle :: (HasTitleVars s, HasCommonVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, HUD m) => m ()
drawTitle = do
  tv <- gets (view titleVars)
  quake <- gets (cvQuake . view commonVars)

  buildingAnimations <- getBuildingAnimations
  let buildingPos = tvBuildingPos tv
  let buildingLoc = Animate.currentLocation buildingAnimations buildingPos
  drawBuilding buildingLoc $ applyQuakeToBuilding quake (0, buildingY)

  drawBackStreet $ applyQuakeToBackStreet quake (0, backStreetY)
  drawStreet $ applyQuakeToStreet quake (0, streetY)

  grassAnimations <- getGrassAnimations
  let grassPos = tvGrassPos tv
  let grassLoc = Animate.currentLocation grassAnimations grassPos
  drawGrass grassLoc $ applyQuakeToGrass quake (0, grassY)

  runnerAnimations <- getRunnerAnimations
  let runnerPos = tvRunnerPos tv
  let runnerLoc = Animate.currentLocation runnerAnimations runnerPos
  drawRunner runnerLoc $ applyQuakeToStreet quake (truncate runnerX, runnerY)

  drawHiscore

  drawTitleText (300, 180)

  when (titleShowPressSpace $ tvFlashing tv) $ drawPressSpaceText (550,500)
  when (titleShowPressEscape $ tvFlashing tv) $ drawPressEscapeText (490,500)