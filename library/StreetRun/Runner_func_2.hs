module StreetRun.Runner_func_2 where


import qualified Safe
import qualified Animate
import Data.Text (Text)
import Linear (V2(..))
import KeyState
import StreetRun.Runner
import StreetRun.Camera
import StreetRun.CFL
import StreetRun.Types
import StreetRun.Hurdle
import StreetRun.Hurdle_1
import StreetRun.Input
import StreetRun.Step
import StreetRun.Physics

runnerX :: Float   -- X position of the runner
runnerX = 200

runnerY :: Num a => a  -- Y position from top 
runnerY = 16 * 26 - 8

duckCamera :: Camera   -- RunnerY is added to make it focus in region near Runner
duckCamera = Camera (V2 ((runnerX + screenWidth) / 2) ((screenHeight + runnerY) / 2)) (V2 2 2)   -- (Point to focus on)(stretch ratio of x and y axis)

rightEdge :: Float
rightEdge = arenaWidth - (runnerX + 48)

runnerHeight :: Maybe Percent -> Int
runnerHeight p = truncate (runnerHeight' p)

runnerHeight' :: Maybe Percent -> Float
runnerHeight' (Just (Percent percent)) = sin (percent * pi) * (-32 * 7) + runnerY
runnerHeight' _ = runnerY

runnerAabb :: Maybe Percent -> Aabb
runnerAabb maybeHeight = Aabb (V2 (runnerX + 4) y) (V2 (runnerX + 24) (y + 48))
  where
    y = runnerHeight' maybeHeight

distanceFromLastObstacle :: [(Float, ObstacleTag)] -> Float
distanceFromLastObstacle obstacles = case Safe.lastMay obstacles of
  Nothing -> rightEdge
  Just (dist, _) -> rightEdge - dist
