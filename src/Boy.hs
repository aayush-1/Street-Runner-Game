module Boy where

import qualified Safe
import qualified Animate
import Data.Text (Text)
import Linear (V2(..))
import KeyState

import BoyRush.Engine.Camera
import BoyRush.Engine.Frame
import BoyRush.Engine.Types
import BoyRush.Engine.Obstacle
import BoyRush.Engine.Input
import BoyRush.Engine.Step
import BoyRush.Engine.Physics

data BoyAction
  = BoyAction'Move
  | BoyAction'Duck
  | BoyAction'Jump
  | BoyAction'Hurt
  deriving (Show, Eq)

data BoyState = BoyState
  { dsAction :: BoyAction
  , dsHeight :: Maybe Percent
  , dsHurt :: Maybe Percent
  , dsRecover :: Maybe Percent
  } deriving (Show, Eq)

data BoyKey
  = BoyKey'Idle
  | BoyKey'Move
  | BoyKey'Kick
  | BoyKey'Hurt
  | BoyKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BoyKey where
  keyName = boyKey'keyName

boyKey'keyName :: BoyKey -> Text
boyKey'keyName = \case
  BoyKey'Idle -> "Idle"
  BoyKey'Move -> "Move"
  BoyKey'Kick -> "Kick"
  BoyKey'Hurt -> "Hurt"
  BoyKey'Sneak -> "Sneak"

boyX :: Float
boyX = 200

boyY :: Num a => a
boyY = 16 * 26 - 8

duckCamera :: Camera
duckCamera = Camera (V2 ((boyX + screenWidth) / 2) ((screenHeight + boyY) / 2)) (V2 2 2)

rightEdge :: Float
rightEdge = arenaWidth - (boyX + 48)

boyHeight :: Maybe Percent -> Int
boyHeight p = truncate (boyHeight' p)

boyHeight' :: Maybe Percent -> Float
boyHeight' (Just (Percent percent)) = sin (percent * pi) * (-32 * 7) + boyY
boyHeight' _ = boyY

boyAabb :: Maybe Percent -> Aabb
boyAabb maybeHeight = Aabb (V2 (boyX + 4) y) (V2 (boyX + 24) (y + 48))
  where
    y = boyHeight' maybeHeight

distanceFromLastObstacle :: [(Float, ObstacleTag)] -> Float
distanceFromLastObstacle obstacles = case Safe.lastMay obstacles of
  Nothing -> rightEdge
  Just (dist, _) -> rightEdge - dist

stepBoyAction :: Input -> BoyState -> Step BoyAction
stepBoyAction input ds = case da of
  BoyAction'Move -> case ksStatus (iUp input) of
    KeyStatus'Pressed -> Step'Change da BoyAction'Jump
    KeyStatus'Held -> Step'Change da BoyAction'Jump
    _ -> case ksStatus (iDown input) of
      KeyStatus'Pressed -> Step'Change da BoyAction'Duck
      KeyStatus'Held -> Step'Change da BoyAction'Duck
      _ -> Step'Sustain BoyAction'Move
  BoyAction'Duck -> case ksStatus (iUp input) of
    KeyStatus'Pressed -> Step'Change da BoyAction'Jump
    KeyStatus'Held -> Step'Change da BoyAction'Jump
    _ -> case ksStatus (iDown input) of
      KeyStatus'Pressed -> Step'Sustain BoyAction'Duck
      KeyStatus'Held -> Step'Sustain BoyAction'Duck
      _ -> Step'Change da BoyAction'Move
  BoyAction'Jump -> case dsHeight ds of
    Nothing -> Step'Change da BoyAction'Move
    Just p -> if p < 1 then Step'Sustain da else Step'Change da BoyAction'Move
  BoyAction'Hurt -> case dsHurt ds of
    Nothing -> Step'Change da BoyAction'Move
    Just p -> if p < 1 then Step'Sustain da else Step'Change da BoyAction'Move
  where
    da = dsAction ds

stepBoyState :: Step BoyAction -> BoyState -> BoyState
stepBoyState stepDa ds = case stepDa of
    Step'Change _ da -> case da of
      BoyAction'Jump -> BoyState da (Just 0) hurt recover
      BoyAction'Hurt -> BoyState da height (Just 0) (Just 0)
      _ -> BoyState nextAction height hurt recover
    Step'Sustain _ -> BoyState nextAction height hurt recover
  where
    nextAction
      | hurt /= Nothing = BoyAction'Hurt
      | height /= Nothing = BoyAction'Jump
      | otherwise = smash stepDa
    height = case dsHeight ds of
      Just p -> if p < 1 then Just (clamp (p + 0.04) 0 1) else Nothing
      Nothing -> Nothing
    hurt = case dsHurt ds of
      Just p -> if p < 1 then Just (clamp (p + 0.04) 0 1) else Nothing
      Nothing -> Nothing
    recover = case dsRecover ds of
      Just p -> if p < 1 then Just (clamp (p + 0.01) 0 1) else Nothing
      Nothing -> Nothing

stepBoyPosition :: Step BoyAction -> Animations BoyKey -> Animate.Position BoyKey Seconds -> Animate.Position BoyKey Seconds
stepBoyPosition (Step'Sustain _) animations pos = Animate.stepPosition animations pos frameDeltaSeconds
stepBoyPosition (Step'Change _ da) _ _ = case da of
  BoyAction'Move -> Animate.initPosition BoyKey'Move
  BoyAction'Duck -> Animate.initPosition BoyKey'Sneak
  BoyAction'Jump -> Animate.initPositionLoops BoyKey'Kick 0
  BoyAction'Hurt -> Animate.initPosition BoyKey'Hurt

stepSpeed :: Step BoyAction -> Percent -> Percent
stepSpeed sda speed = clamp speed' 1 20
  where
    da = smash sda
    speed' = case da of
      BoyAction'Duck -> speed - 0.1
      BoyAction'Move -> speed + 0.03
      BoyAction'Jump -> speed
      BoyAction'Hurt -> speed - 0.15

showBoy :: BoyState -> Bool
showBoy BoyState{dsRecover} = case dsRecover of
  Nothing -> True
  Just percent -> sin (1000 * (percent ** 3)) >= 0

addStocks :: Score -> Score -> Bool
addStocks s s' = or [pass 1, pass 5, pass 10, pass 20, pass 35, pass 50, pass 75, powerOf50]
  where
    pass n = s < n && s' >= n
    powerOf50 = s' >= 100 && mod s 50 > mod s' 50

nextStocks :: Score -> Score -> Stocks -> Stocks
nextStocks s s' st = min 10 (st + if addStocks s s' then 1 else 0)

stepZoom :: Float -> BoyAction -> Float
stepZoom zoom boyAction = case boyAction of
  BoyAction'Duck -> clamp (zoom - 0.01) 0 1
  _ -> clamp (zoom + 0.05) 0 1

applyHurt :: Bool -> Step BoyAction -> Maybe Percent -> Step BoyAction
applyHurt collision stepDa recover
  | collision && recover == Nothing = case stepDa of
      Step'Sustain BoyAction'Hurt -> stepDa
      Step'Sustain da -> Step'Change da BoyAction'Hurt
      Step'Change da _ -> Step'Change da BoyAction'Hurt
  | otherwise = stepDa
