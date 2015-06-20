module Wrench.Filesystem(
    getFilesInDir
  , getFilesWithExtInDir
  ) where

import           ClassyPrelude hiding(FilePath,(</>))
import           System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath
import           Control.Category          ((>>>))

getDirectoryContentsWrapped :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContentsWrapped fp = liftIO (getDirectoryContents fp)

doesFileExistWrapped :: MonadIO m => FilePath -> m Bool
doesFileExistWrapped fp = liftIO (doesFileExist fp)

type Extension = String

getFilesWithExtInDir :: (Functor m,MonadIO m) => FilePath -> Extension -> m [FilePath]
getFilesWithExtInDir dir ext = filter (takeExtension >>> (== ext)) <$> getFilesInDir dir

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: MonadIO m => FilePath -> m [FilePath]
getFilesInDir dir = do
  result <- getDirectoryContentsWrapped dir >>= (return . map (dir </>)) >>= filterM doesFileExistWrapped
  return result
