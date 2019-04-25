module StreetRun.Shaky where


import Control.Lens
import Control.Monad.State (MonadState(..), modify, gets)

import StreetRun.Sound_effects
import StreetRun.Common
import StreetRun.Quake
import StreetRun.Quake_Building
import StreetRun.Quake_BackStreet
import StreetRun.Quake_Street
import StreetRun.Quake_Grass
import StreetRun.Step
import StreetRun.Sfx

updateQuake :: (AudioSfx m, MonadState s m, HasCommonVars s) => m ()
updateQuake = do
  sq <- stepQuake <$> gets (cvQuake . view commonVars)
  addSfxs $ if startQuake sq then [Sfx'Quake] else []
  modify $ commonVars %~ (\cv -> cv{ cvQuake = smash sq })
