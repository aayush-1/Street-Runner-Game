{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Runner where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..))
import KeyState

import StreetRun.Config
import StreetRun.Effect.Audio
import StreetRun.Effect.Camera
import StreetRun.Effect.Clock
import StreetRun.Effect.Logger
import StreetRun.Effect.Renderer
import StreetRun.Effect.Sfx
import StreetRun.Effect.Quake
import StreetRun.Engine.Camera
import StreetRun.Engine.Input
import StreetRun.Engine.Frame
import StreetRun.Engine.Play
import StreetRun.Engine.Title
import StreetRun.Manager.Input
import StreetRun.Manager.Scene
import StreetRun.Scene.GameOver
import StreetRun.Scene.Play
import StreetRun.Scene.Death
import StreetRun.Scene.Pause
import StreetRun.Scene.Title
import StreetRun.State

titleTransition :: (HasTitleVars a, MonadState a m, CameraControl m) => m ()
titleTransition = do
  adjustCamera initCamera
  modify $ titleVars .~ initTitleVars

playTransition :: (HasPlayVars a, MonadState a m, Audio m) => m ()
playTransition = do
  PlayVars{pvUpcomingObstacles} <- gets (view playVars)
  modify $ playVars .~ (initPlayVars pvUpcomingObstacles)
  playGameMusic

deathTransition :: (Audio m) => m ()
deathTransition = do
  stopGameMusic
  playDeathSfx

pauseToPlay :: Audio m => m ()
pauseToPlay = raiseGameMusic

playToPause :: Audio m => m ()
playToPause = lowerGameMusic

toScene' :: MonadState Vars m => Scene -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

mainLoop ::
  ( MonadReader Config m
  , MonadState Vars m
  , Audio m
  , AudioSfx m
  , Logger m
  , Clock m
  , CameraControl m
  , Renderer m
  , HasInput m
  , Title m
  , Play m
  , Pause m
  , Death m
  , GameOver m
  ) => m ()
mainLoop = do
  updateInput
  input <- getInput
  clearScreen
  clearSfx
  scene <- gets vScene
  updateQuake
  step scene
  playSfx
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  nextScene <- gets vNextScene
  stepScene scene nextScene
  let quit = nextScene == Scene'Quit || iQuit input || ksStatus (iEscape input) == KeyStatus'Pressed
  unless quit mainLoop
  where

    step scene = do
      case scene of
        Scene'Title -> titleStep
        Scene'Play -> playStep
        Scene'Pause -> pauseStep
        Scene'Death -> deathStep
        Scene'GameOver -> gameOverStep
        Scene'Quit -> return ()

    stepScene scene nextScene = do
      when (nextScene /= scene) $ do
        case nextScene of
          Scene'Title -> titleTransition
          Scene'Play -> case scene of
            Scene'Title -> playTransition
            Scene'Pause -> pauseToPlay
            _ -> return ()
          Scene'Death -> case scene of
            Scene'Play -> deathTransition
            _ -> return ()
          Scene'Pause -> case scene of
            Scene'Play -> playToPause
            _ -> return ()
          Scene'GameOver -> return ()
          Scene'Quit -> return ()
        modify (\v -> v { vScene = nextScene })
