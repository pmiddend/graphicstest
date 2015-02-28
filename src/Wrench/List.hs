module Wrench.List(concatMapM) where

import ClassyPrelude

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)
