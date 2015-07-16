{-# LANGUAGE TemplateHaskell #-}
module Wrench.WindowSize where

import Control.Lens(makePrisms)
import ClassyPrelude

data WindowSize = DynamicWindowSize
                | ConstantWindowSize Int Int

$(makePrisms ''WindowSize)
