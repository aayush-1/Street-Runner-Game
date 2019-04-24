module StreetRun.Effect.Quake where

import Control.Lens
import Control.Monad.State (MonadState(..), modify, gets)

import StreetRun.Effect.Sfx
import StreetRun.Engine.Common
import StreetRun.Engine.Quake
import StreetRun.Engine.Step
import StreetRun.Engine.Sfx

updateQuake :: (AudioSfx m, MonadState s m, HasCommonVars s) => m ()
updateQuake = do
  sq <- stepQuake <$> gets (cvQuake . view commonVars)
  addSfxs $ if startQuake sq then [Sfx'Quake] else []
  modify $ commonVars %~ (\cv -> cv{ cvQuake = smash sq })
