module StreetRun
  ( main
  ) where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import qualified Data.Text.IO as T

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Exception.Safe (MonadThrow, MonadCatch)
import SDL.Vect
import System.Random

import StreetRun.Config
import StreetRun.Audio
import StreetRun.Picture
import StreetRun.Clock
import StreetRun.Logger
import StreetRun.Renderer
import StreetRun.Renderer_func_1
import StreetRun.Renderer_func_2
import StreetRun.HUD
import StreetRun.Sound_effects
import StreetRun.Hurdle
import StreetRun.Hurdle_1
import StreetRun.SDLLayer
import StreetRun.Load
import StreetRun.Scene
import StreetRun.Resource
import StreetRun.Bolt
import StreetRun.Bolt_1
import StreetRun.Title
import StreetRun.Pause
import StreetRun.Play
import StreetRun.Death
import StreetRun.GameOver
import StreetRun.State

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
  window <- SDL.createWindow "Street Runner" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  mkObstacles <- streamOfObstacles <$> getStdGen
  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        }
  runStreetRun cfg (initVars mkObstacles) mainLoop
  SDL.destroyWindow window
  freeResources resources
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  SDL.quit

newtype StreetRun a = StreetRun (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO, MonadThrow, MonadCatch)

runStreetRun :: Config -> Vars -> StreetRun a -> IO a
runStreetRun config v (StreetRun m) = evalStateT (runReaderT m config) v

instance Audio StreetRun where
  playGameMusic = playGameMusic'
  stopGameMusic = stopGameMusic'
  playJumpSfx = playJumpSfx'
  playDuckSfx = playDuckSfx'
  playPointSfx = playPointSfx'
  playBirdSfx = playBirdSfx'
  playHurtSfx = playHurtSfx'
  playLavaSfx = playLavaSfx'
  playQuakeSfx = playQuakeSfx'
  playRockSfx = playRockSfx'
  playRecoverSfx = playRecoverSfx'
  playDeathSfx = playDeathSfx'
  lowerGameMusic = lowerGameMusic'
  raiseGameMusic = raiseGameMusic'
  playStockSfx = playStockSfx'

instance Clock StreetRun where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger StreetRun where
  logText = liftIO . T.putStrLn

instance SDLRenderer StreetRun where
  drawTexture = drawTexture'
  presentRenderer = presentRenderer'
  clearRenderer = clearRenderer'
  queryTexture = queryTexture'

instance SDLInput StreetRun where
  pollEventPayloads = pollEventPayloads'

instance HasInput StreetRun where
  updateInput = updateInput'
  getInput = getInput'
  setInput = setInput'

instance SceneManager StreetRun where
  toScene = toScene'

instance Renderer StreetRun where
  clearScreen = clearScreen'
  drawScreen = drawScreen'
  getRunnerAnimations = getSpriteAnimations (rRunnerSprites . cResources)
  getLavaAnimations = getSpriteAnimations (rLavaSprites . cResources)
  getRockAnimations = getSpriteAnimations (rRockSprites . cResources)
  getBirdAnimations = getSpriteAnimations (rBirdSprites . cResources)
  getBuildingAnimations = getSpriteAnimations (rBuildingSprites . cResources)
  getGrassAnimations = getSpriteAnimations (rGrassSprites . cResources)
  drawRunner = drawSprite (rRunnerSprites . cResources)
  drawLava = drawSprite (rLavaSprites . cResources)
  drawRock = drawSprite (rRockSprites . cResources)
  drawBird = drawSprite (rBirdSprites . cResources)
  drawBuilding = drawHorizontalScrollSprite (rBuildingSprites . cResources) 16
  drawBackStreet = drawHorizontalScrollImage (rBackStreetSprites . cResources) 8
  drawStreet = drawHorizontalScrollImage (rStreetSprites . cResources) 2
  drawGrass = drawHorizontalScrollSprite (rGrassSprites . cResources) 4
  drawBlackOverlay = drawBlackOverlay'
  drawHiscoreText = drawTextureSprite (rHiscoreSprite . cResources)
  drawPauseText = drawTextureSprite (rPauseSprite . cResources)
  drawGameOverText = drawTextureSprite (rGameOverSprite . cResources)
  drawPressSpaceText = drawTextureSprite (rSpaceSprite . cResources)
  drawPressEscapeText = drawTextureSprite (rEscapeSprite . cResources)
  drawTitleText = drawTextureSprite (rTitleSprite . cResources)
  drawNumber n = drawTextureSprite (flip rNumberSprites n . cResources)
  drawControlsText = drawTextureSprite (rControlsSprite . cResources)

instance Title StreetRun where
  titleStep = titleStep'

instance Play StreetRun where
  playStep = playStep'

instance Pause StreetRun where
  pauseStep = pauseStep'

instance Death StreetRun where
  deathStep = deathStep'

instance GameOver StreetRun where
  gameOverStep = gameOverStep'

instance CameraControl StreetRun where
  adjustCamera = adjustCamera'

instance HUD StreetRun where
  drawScore = drawScore'
  drawHiscore = drawHiscore'
  drawControls = drawControls'

instance AudioSfx StreetRun where
  playSfx = playSfx'
  clearSfx = clearSfx'
  addSfxs = addSfxs'
