{-|
Module      : Wrench.SoundMap
Description : Map sound identifiers to sounds
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.SoundMap where

import qualified Data.Map.Strict        as M
import           Wrench.SoundIdentifier

type SoundMap a = M.Map SoundIdentifier a
