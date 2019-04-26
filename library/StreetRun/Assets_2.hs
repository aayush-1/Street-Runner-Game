module StreetRun.Assets_2 where

import StreetRun.Assets_1
import StreetRun.Assets
import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import qualified SDL.Image as Image
import qualified Animate
import Data.Text.Conversions (toText)
import Data.StateVar (($=))
import SDL.Vect
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered

import StreetRun.Types
import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.Font
import StreetRun.Font_functions
import StreetRun.Snag
import StreetRun.Scenario

freeResources :: Resources -> IO ()
freeResources r = do
  SDL.destroyTexture $ Animate.ssImage (rBuildingSprites r)
  SDL.destroyTexture (rBackStreetSprites r)
  SDL.destroyTexture (rStreetSprites r)
  SDL.destroyTexture $ Animate.ssImage (rGrassSprites r)
  SDL.destroyTexture $ Animate.ssImage (rRunnerSprites r)
  SDL.destroyTexture $ Animate.ssImage (rBirdSprites r)
  SDL.destroyTexture $ Animate.ssImage (rLavaSprites r)
  SDL.destroyTexture $ Animate.ssImage (rRockSprites r)
  SDL.destroyTexture (rPauseSprite r)
  SDL.destroyTexture (rEscapeSprite r)
  SDL.destroyTexture (rSpaceSprite r)
  SDL.destroyTexture (rGameOverSprite r)
  SDL.destroyTexture (rHiscoreSprite r)
  SDL.destroyTexture (rTitleSprite r)
  SDL.destroyTexture (rControlsSprite r)

  Mixer.free (rGameMusic r)
  Mixer.free (rJumpSfx r)
  Mixer.free (rDuckSfx r)
  Mixer.free (rPointSfx r)
  Mixer.free (rBirdSfx r)
  Mixer.free (rHurtSfx r)
  Mixer.free (rLavaSfx r)
  Mixer.free (rQuakeSfx r)
  Mixer.free (rRockSfx r)
  Mixer.free (rRecoverSfx r)
  Mixer.free (rStockSfx r)
  Mixer.free (rDeathSfx r)
