module StreetRun.Hurdle where

import qualified Animate
import System.Random
import Linear (V2(..))

import StreetRun.Types -- to import new types defined in that module
import StreetRun.Bird
import StreetRun.Lava
import StreetRun.Rock
import StreetRun.Physics

data ObstacleTag
  = ObstacleTag'Lava    --nullary constructors 
  | ObstacleTag'Rock
  | ObstacleTag'Bird
  deriving (Show, Eq, Ord, Bounded, Enum) --hence deriving Enum 

instance Random ObstacleTag where
  randomR = randomRBoundedEnum
  random g = randomR (minBound, maxBound) g

data ObstacleInfo
  = ObstacleInfo'Lava (Animate.Position LavaKey Seconds) --seconds will function just like float (defined in types in module)
  | ObstacleInfo'Rock (Animate.Position RockKey Seconds) 
  | ObstacleInfo'Bird (Animate.Position BirdKey Seconds)
  deriving (Show, Eq)

data ObstacleState = ObstacleState
  { osInfo :: ObstacleInfo
  , osDistance :: Distance -- again defined in types 
  } deriving (Show, Eq)

lavaY, rockY, birdY :: Num a => a -- dist of obstacle from top 
lavaY = 16 * 28
rockY = 16 * 26 + 2
birdY = 16 * 22   

obstacleAabb :: ObstacleState -> Aabb -- affects collision of obstacles with runner 
obstacleAabb ObstacleState{osInfo,osDistance} = case osInfo of
  ObstacleInfo'Lava _ -> Aabb (V2 (0 + dist) lavaY) (V2 (32 + dist) (lavaY + 32))
  ObstacleInfo'Rock _ -> Aabb (V2 (0 + dist) rockY) (V2 (32 + dist) (rockY + 32))
  ObstacleInfo'Bird _ -> Aabb (V2 (0 + dist) birdY) (V2 (32 + dist) (birdY + 32)) --(obleft obtop)(obright obdown)
  where
    dist = realToFrac osDistance

randomRBoundedEnum :: (Bounded a, Enum a, RandomGen g) => (a, a) -> g -> (a, g)
randomRBoundedEnum (aMin, aMax) g = let
  (index, g') = randomR (fromEnum aMin, fromEnum aMax) g
  lastEnum = maxBound
  a = [minBound..lastEnum] !! (index `mod` (fromEnum lastEnum + 1))
  in (a, g')

streamOfObstacles :: RandomGen g => g -> [(Int, ObstacleTag)]
streamOfObstacles g = zip (map (\dist -> dist `mod` 18+3) $ randoms g) (randoms g) -- dist%18 captures distance between obstacles arriving 

stepObstacles :: Distance -> [ObstacleState] -> [ObstacleState] --speed of bird flying
stepObstacles delta = map
  (\o@ObstacleState{osInfo, osDistance} -> o { osDistance = osDistance - (case osInfo of ObstacleInfo'Bird _ -> delta + 4; _ -> delta) })
