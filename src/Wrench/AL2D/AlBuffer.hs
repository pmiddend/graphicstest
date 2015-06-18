module Wrench.AL2D.AlBuffer where

import Sound.AL.Types(ALuint)

newtype AlBuffer = AlBuffer { alBufferImpl :: ALuint }
