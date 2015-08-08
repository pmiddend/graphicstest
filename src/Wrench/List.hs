{-|
Module      : Wrench.List
Description : General list utilities
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.List(concatMapM) where

import           ClassyPrelude

-- | Combination of 'mapM' and 'concat'
concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)
