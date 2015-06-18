module Wrench.AL2D.AlSource where
        
import Sound.AL.Types(ALuint)

newtype AlSource = AlSource { alSourceImpl :: ALuint }
