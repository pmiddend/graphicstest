{-# LANGUAGE CPP #-}
module Wrench.AL2D.AlBuffer where

#ifdef USE_OPENAL
import Sound.AL.Types(ALuint)

newtype AlBuffer = AlBuffer { alBufferImpl :: ALuint }
#endif
