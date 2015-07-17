{-# LANGUAGE CPP #-}
module Wrench.AL2D.AlSource where

#ifdef USE_OPENAL        
import Sound.AL.Types(ALuint)

newtype AlSource = AlSource { alSourceImpl :: ALuint }
#endif
