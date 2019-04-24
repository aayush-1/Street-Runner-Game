module StreetRun.Effect.Sfx where

import Control.Lens
import Control.Monad.State

import StreetRun.Engine.Sfx
import StreetRun.Engine.Common
import StreetRun.Effect.Audio

class Monad m => AudioSfx m where
  clearSfx :: m ()
  addSfxs :: [Sfx] -> m ()
  playSfx :: m ()

clearSfx' :: (HasCommonVars s, MonadState s m) => m ()
clearSfx' = modify $ commonVars %~ (\cv -> cv { cvSfx = [] }) -- clear the list of sommon variable sfxs

addSfxs' :: (HasCommonVars s, MonadState s m) => [Sfx] -> m ()
addSfxs' sfxs = modify $ commonVars %~ (\cv -> cv { cvSfx = sfxs ++ cvSfx cv })

playSfx' :: (Audio m, HasCommonVars s, MonadState s m) => m () --plays particular sfx 
playSfx' = do
  CommonVars{cvSfx} <- gets (view commonVars)
  forM_ cvSfx $ \sfx -> case sfx of
    Sfx'Jump -> playJumpSfx
    Sfx'Duck -> playDuckSfx
    Sfx'Point -> playPointSfx
    Sfx'Bird -> playBirdSfx
    Sfx'Hurt -> playHurtSfx
    Sfx'Lava -> playLavaSfx
    Sfx'Quake -> playQuakeSfx
    Sfx'Rock -> playRockSfx
    Sfx'Recover -> playRecoverSfx
    Sfx'Stock -> playStockSfx
