module StreetRun.Engine.Input where
 
import KeyState

data Input = Input
  { iSpace :: KeyState Int --comprises 4 constructors (untouched,Pressed,Held,Released)
  , iUp :: KeyState Int
  , iDown :: KeyState Int
  , iEscape :: KeyState Int
  , iQuit :: Bool --quit initially set to false 
  } deriving (Show, Eq)

initInput :: Input
initInput = Input initKeyState initKeyState initKeyState initKeyState False
