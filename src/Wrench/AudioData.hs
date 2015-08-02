{-|
Module      : Wrench.AudioData
Description : Load audio file descriptions from a text file
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.AudioData(
  SoundIdentifier,
  SoundMap,
  readAudioFiles
  ) where

import           ClassyPrelude          hiding (FilePath)
import qualified Data.Map.Strict        as M
import           System.FilePath
import           Wrench.Filesystem
import           Wrench.SoundIdentifier
import           Wrench.SoundMap

loadSoundTuple :: (Applicative m,Functor m,Monad m) => (FilePath -> m a) -> FilePath -> m (SoundIdentifier,a)
loadSoundTuple loader fn = (,) <$> (pure . pack . dropExtension . takeFileName $ fn) <*> loader fn

readAudioFiles :: (Applicative m,Functor m,MonadIO m) => (FilePath -> m a) -> FilePath -> m (SoundMap a)
readAudioFiles loader mediaPath = do
  files <- getFilesInDir mediaPath
  M.fromList <$> (traverse (loadSoundTuple loader) files)



