module StreetRun.Scene_change where

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'Death
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)
