module Wrench.AL2D.AlBufferFormat where

import ClassyPrelude

data AlBufferFormat = AlBufferFormatMono8
                    | AlBufferFormatMono16
                    | AlBufferFormatStereo8
                    | AlBufferFormatStereo16
                    deriving(Show,Eq)
