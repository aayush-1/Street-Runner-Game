module StreetRun.Font where

data Number
  = Number'0
  | Number'1
  | Number'2
  | Number'3
  | Number'4
  | Number'5
  | Number'6
  | Number'7
  | Number'8
  | Number'9
  deriving (Show, Eq, Enum, Bounded) -- sequentially enumerated (0,1,2,3,4,5,6,7,8,9)

  