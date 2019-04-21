{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Scene.Play where

import qualified Animate
import Control.Monad (when)
import Control.Lens hiding (zoom)
import Control.Monad.State (MonadState(..), modify, gets)
import Data.Foldable (forM_)
import KeyState

import StreetRun.Effect.Audio
import StreetRun.Effect.Clock
import StreetRun.Effect.Camera
import StreetRun.Effect.Logger
import StreetRun.Effect.HUD
import StreetRun.Effect.Renderer
import StreetRun.Effect.Sfx
import StreetRun.Engine.Common
import StreetRun.Engine.Input
import StreetRun.Engine.Camera
import StreetRun.Engine.Frame
import StreetRun.Engine.Step
import StreetRun.Engine.Runner
import StreetRun.Engine.Obstacle
import StreetRun.Engine.Play
import StreetRun.Engine.Sfx
import StreetRun.Engine.Quake
import StreetRun.Engine.Physics
import StreetRun.Manager.Scene
import StreetRun.Manager.Input

class Monad m => Play m where
  playStep :: m ()

playStep' :: (HasPlayVars s, HasCommonVars s, MonadState s m, Logger m, CameraControl m, Clock m, Renderer m, Audio m, AudioSfx m, HasInput m, SceneManager m, HUD m) => m ()
playStep' = do
  input <- getInput
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Pause)
  updatePlay
  drawPlay

updatePlay :: (HasPlayVars s, HasCommonVars s, MonadState s m, Logger m, Clock m, CameraControl m, Renderer m, HasInput m, AudioSfx m, SceneManager m) => m ()
updatePlay = do
  input <- getInput
  da <- (stepRunnerAction input . pvRunnerState) <$> gets (view playVars)
  updateSeconds
  updateSpeed da
  updateObstacles
  (collision, da') <- tryCollision da
  updateZoom da'
  updateRunner da'
  updateCamera
  updateScrolling
  updateStocks collision
  updateHiscore
  isDead <- getDead
  when isDead (toScene Scene'Death)

drawPlay :: (HasPlayVars s, HasCommonVars s, MonadState s m, Renderer m, CameraControl m, HUD m) => m ()
drawPlay = do
  runnerAnimations <- getRunnerAnimations
  buildingAnimations <- getBuildingAnimations
  grassAnimations <- getGrassAnimations
  quake <- gets (cvQuake . view commonVars)
  pv <- gets (view playVars)
  let runnerLoc = Animate.currentLocation runnerAnimations (pvRunnerPos pv)
  let buildingLoc = Animate.currentLocation buildingAnimations (pvBuildingPos pv)
  let grassLoc = Animate.currentLocation grassAnimations (pvGrassPos pv)
  drawBuilding buildingLoc $ applyQuakeToBuilding quake (truncate $ pvBuildingScroll pv, buildingY)
  drawBackStreet $ applyQuakeToBackStreet quake (truncate $ pvBackStreetScroll pv, backStreetY)
  drawStreet $ applyQuakeToStreet quake (truncate $ pvStreetScroll pv, streetY)
  when (pvShowRunner pv) $ drawRunner runnerLoc $ applyQuakeToStreet quake (truncate runnerX, runnerHeight (dsHeight $ pvRunnerState pv))
  drawObstacles quake (pvObstacles pv)
  drawGrass grassLoc $ applyQuakeToGrass quake (truncate $ pvGrassScroll pv, grassY)
  disableZoom
  drawControls
  drawStocks pv runnerAnimations
  drawHiscore
  drawScore
  enableZoom
  where
    drawStocks pv runnerAnimations =
      flip mapM_ [1..(fromIntegral $ pvStocks pv - 1)] $ \stock -> do
        let idleLoc = Animate.currentLocation runnerAnimations (Animate.initPosition RunnerKey'Kick)
        drawRunner idleLoc (20 + 48 * (stock - 1), 32)

drawObstacles :: Renderer m => Quake -> [ObstacleState] -> m ()
drawObstacles quake obstacles = do
  lavaAnimations <- getLavaAnimations
  rockAnimations <- getRockAnimations
  birdAnimations <- getBirdAnimations
  forM_ obstacles $ \ObstacleState{osInfo,osDistance} -> let
    x = truncate osDistance
    in case osInfo of
      ObstacleInfo'Lava pos -> drawLava (Animate.currentLocation lavaAnimations pos) $ applyQuakeToStreet quake (x, lavaY)
      ObstacleInfo'Rock pos -> drawRock (Animate.currentLocation rockAnimations pos) $ applyQuakeToStreet quake (x, rockY)
      ObstacleInfo'Bird pos -> drawBird (Animate.currentLocation birdAnimations pos) $ applyQuakeToStreet quake (x, birdY)

detectCollision :: [ObstacleState] -> RunnerState -> Bool
detectCollision obstacles runnerState = or $ flip map obstacles $ \obs ->
  collisionIntersect (runnerAabb (dsHeight runnerState)) (obstacleAabb obs)

modifyPlayVars :: (MonadState s m, HasPlayVars s) => (PlayVars -> PlayVars) -> m ()
modifyPlayVars f = modify $ playVars %~ f

updateSeconds :: (MonadState s m, HasPlayVars s) => m ()
updateSeconds = modifyPlayVars $ \pv -> pv { pvSeconds = pvSeconds pv + frameDeltaSeconds }

updateSpeed :: (MonadState s m, HasPlayVars s) => Step RunnerAction -> m ()
updateSpeed da = modifyPlayVars $ \pv -> pv { pvSpeed = stepSpeed da (pvSpeed pv) }

updateRunner :: (MonadState s m, HasPlayVars s, Renderer m, AudioSfx m) => Step RunnerAction -> m ()
updateRunner sda = do
  runnerAnimations <- getRunnerAnimations
  let sfx = case sda of
        Step'Sustain _ -> []
        Step'Change da da' -> case da' of
          RunnerAction'Jump -> [Sfx'Jump]
          RunnerAction'Duck -> [Sfx'Duck]
          RunnerAction'Hurt -> [Sfx'Hurt]
          RunnerAction'Move -> case da of
            RunnerAction'Hurt -> [Sfx'Recover]
            _ -> []
  addSfxs sfx
  modifyPlayVars $ \pv -> let
    ds = stepRunnerState sda (pvRunnerState pv)
    in pv
      { pvRunnerState = ds
      , pvShowRunner = showRunner ds
      , pvRunnerPos = stepRunnerPosition sda runnerAnimations (pvRunnerPos pv)
      }

updateZoom :: (MonadState s m, HasPlayVars s) => Step RunnerAction -> m ()
updateZoom da = modifyPlayVars $ \pv -> pv { pvZoom = stepZoom (pvZoom pv) (smash da) }

updateCamera :: (MonadState s m, HasPlayVars s, CameraControl m) => m ()
updateCamera = do
  zoom <- gets (pvZoom . view playVars)
  let cam = lerpCamera ((1 - zoom) ** (1.8 :: Float)) duckCamera initCamera
  adjustCamera cam

updateObstacles :: (MonadState s m, HasPlayVars s, Renderer m, AudioSfx m) => m ()
updateObstacles = do
  PlayVars{pvUpcomingObstacles,pvObstacles,pvSpeed,pvScore} <- gets (view playVars)
  lavaAnimations <- getLavaAnimations
  rockAnimations <- getRockAnimations
  birdAnimations <- getBirdAnimations
  let (obstacles, removedCount, upcomingObstacles, newObstacleTag) = iterateObstacles pvUpcomingObstacles pvSpeed pvObstacles
  let pointSfx = if removedCount > 0 then [Sfx'Point] else []
  let obstacleSfx = case newObstacleTag of
        Nothing -> []
        Just o -> case o of
          ObstacleTag'Lava -> [Sfx'Lava]
          ObstacleTag'Rock -> [Sfx'Rock]
          ObstacleTag'Bird -> [Sfx'Bird]
  let updateObstaclePos oi = case oi of
        ObstacleInfo'Lava pos -> ObstacleInfo'Lava $ Animate.stepPosition lavaAnimations pos frameDeltaSeconds
        ObstacleInfo'Rock pos -> ObstacleInfo'Rock $ Animate.stepPosition rockAnimations pos frameDeltaSeconds
        ObstacleInfo'Bird pos -> ObstacleInfo'Bird $ Animate.stepPosition birdAnimations pos frameDeltaSeconds
  let score = pvScore + fromIntegral removedCount
  let stockSfx = if addStocks pvScore score then [Sfx'Stock] else []
  addSfxs $ pointSfx ++ obstacleSfx ++ stockSfx
  modifyPlayVars $ \pv -> let
    in pv
      { pvObstacles = map (\os -> os { osInfo = updateObstaclePos (osInfo os) }) obstacles
      , pvScore = score
      , pvUpcomingObstacles = upcomingObstacles
      , pvStocks = nextStocks pvScore score (pvStocks pv)
      }

tryCollision :: (MonadState s m, HasPlayVars s) => Step RunnerAction -> m (Bool, Step RunnerAction)
tryCollision da = do
  pv <- gets (view playVars)
  let collision = detectCollision (pvObstacles pv) (pvRunnerState pv) && dsRecover (pvRunnerState pv) == Nothing
  let da' = applyHurt collision da (dsRecover (pvRunnerState pv))
  return (collision, da')

updateScrolling :: (Renderer m, HasPlayVars s, MonadState s m) => m ()
updateScrolling = do
  buildingAnimations <- getBuildingAnimations
  grassAnimations <- getGrassAnimations
  modifyPlayVars $ \pv -> let
    speed = pvSpeed pv
    in pv
      { pvBuildingPos = Animate.stepPosition buildingAnimations (pvBuildingPos pv) frameDeltaSeconds
      , pvGrassPos = Animate.stepPosition grassAnimations (pvGrassPos pv) frameDeltaSeconds
      , pvBuildingScroll = stepHorizontalDistance (realToFrac $ pvBuildingScroll pv) (realToFrac (-speed) / 3)
      , pvBackStreetScroll = stepHorizontalDistance (realToFrac $ pvBackStreetScroll pv) (realToFrac (-speed) / 2)
      , pvStreetScroll = stepHorizontalDistance (realToFrac $ pvStreetScroll pv) (realToFrac (-speed))
      , pvGrassScroll = stepHorizontalDistance (realToFrac $ pvGrassScroll pv) (realToFrac (-speed) * 1.5)
      }

updateStocks :: (MonadState s m, HasPlayVars s) => Bool -> m ()
updateStocks collision = modifyPlayVars $ \pv -> pv { pvStocks = pvStocks pv - (if collision then 1 else 0) }

updateHiscore :: (MonadState s m, HasCommonVars s, HasPlayVars s) => m ()
updateHiscore = do
  score <- gets (pvScore . view playVars)
  modify $ commonVars %~ \cv -> cv { cvHiscore = max (cvHiscore cv) score }

getDead :: (MonadState s m, HasPlayVars s) => m Bool
getDead = (<= 0) <$> gets (pvStocks . view playVars)