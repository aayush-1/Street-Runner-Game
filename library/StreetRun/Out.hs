{-# LANGUAGE TemplateHaskell #-} -- '' is included 
module StreetRun.Out where

import Control.Lens

import StreetRun.Types

data GameOverVars = GameOverVars
  { govFadeout :: Percent --defined in types module 
  , govSpaceFlashing :: Percent
  } deriving (Show, Eq)

