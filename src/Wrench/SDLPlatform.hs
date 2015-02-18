module Wrench.SDLPlatform where

import Wrench.Platform

data SDLPlatform = SDLPlatform

instance Platform SDLPlatform where
  withPlatform windowTitle backgroundColor callback = 
