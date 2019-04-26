{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Bolt where

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

