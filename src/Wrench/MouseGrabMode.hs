{-# LANGUAGE TemplateHaskell #-}
module Wrench.MouseGrabMode where

import           ClassyPrelude
import           Control.Lens  (makePrisms)

data MouseGrabMode = MouseGrabNo
                   | MouseGrabYes
                   deriving(Show,Read,Eq,Bounded,Enum)

$(makePrisms ''MouseGrabMode)
