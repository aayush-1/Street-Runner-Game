{-# LANGUAGE TemplateHaskell #-} -- '' is included 
module StreetRun.Out_func where
import Control.Lens
import StreetRun.Types
import StreetRun.Out

makeClassy ''GameOverVars --'' prepend to any type ot class turns it into name 
						  -- Useing Lenses to ease the access of Nested structures 

initGameOverVars :: GameOverVars
initGameOverVars = GameOverVars 0 0

stepGameOverVars :: GameOverVars -> GameOverVars
stepGameOverVars gov = gov
  { govFadeout = clamp (govFadeout gov + 0.01) 0 0 --from normal till extent we want fading to occur (with given rate)
  , govSpaceFlashing = govSpaceFlashing gov + 0.05 --speed of press space on screen
  }

gameOverShowPressSpace :: Percent -> Bool
gameOverShowPressSpace p = sin p < 0
