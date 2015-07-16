{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
module Wrench.MediaData where

import           ClassyPrelude hiding(FilePath,(</>))
import Wrench.ImageData
import Control.Lens(makeLenses)
import qualified Data.Map.Strict           as M
import System.FilePath

data MediaData a = MediaData {
    _mdSurfaces :: SurfaceMap a
  , _mdAnims :: AnimMap
  }

$(makeLenses ''MediaData)

readMediaFiles :: forall a m.(Applicative m, MonadIO m) => ImageLoadFunction m a -> FilePath -> m (MediaData a)
readMediaFiles loadImage fp = MediaData <$> (foldr M.union M.empty <$> smaps) <*> (foldr M.union M.empty <$> amaps)
  where readSingle :: ImageDescFile -> m (SurfaceMap a,AnimMap)
        readSingle f = imageDescToSurface loadImage f >>= imageDescToMaps f
        maps :: m [(SurfaceMap a,AnimMap)]
        maps = getDescFilesInDir fp >>= traverse readSingle
        smaps :: m [SurfaceMap a]
        smaps = map fst <$> maps
        amaps :: m [AnimMap]
        amaps = map snd <$> maps
