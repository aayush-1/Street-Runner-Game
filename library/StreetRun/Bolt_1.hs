{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Bolt_1 where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..))
import KeyState

import StreetRun.Config
import StreetRun.Audio
import StreetRun.Picture
import StreetRun.CFL
import StreetRun.Renderer
import StreetRun.Renderer_func_1
import StreetRun.Renderer_func_2
import StreetRun.Bolt
import StreetRun.Sound_effects
import StreetRun.Shaky
import StreetRun.Camera
import StreetRun.Input
import StreetRun.Work
import StreetRun.Work_func
import StreetRun.Tag
import StreetRun.Tag_func
import StreetRun.Load
import StreetRun.Scene
import StreetRun.GameOver
import StreetRun.Play
import StreetRun.Death
import StreetRun.Pause
import StreetRun.Title
import StreetRun.State

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
