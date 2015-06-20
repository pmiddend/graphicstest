module Wrench.AudioData(
  SoundId,
  SoundMap,
  readAudioFiles
  ) where

import ClassyPrelude hiding (FilePath)
import Wrench.Filesystem
import System.FilePath
import qualified Data.Map.Strict as M

type SoundId = Text
type SoundMap a = M.Map SoundId a

loadSoundTuple :: (Applicative m,Functor m,Monad m) => (FilePath -> m a) -> FilePath -> m (SoundId,a)
loadSoundTuple loader fn = (,) <$> (pure . pack . dropExtension . takeFileName $ fn) <*> loader fn

readAudioFiles :: (Applicative m,Functor m,MonadIO m) => (FilePath -> m a) -> FilePath -> m (SoundMap a)
readAudioFiles loader mediaPath = do
  files <- getFilesInDir mediaPath
  M.fromList <$> (traverse (loadSoundTuple loader) files)


       
