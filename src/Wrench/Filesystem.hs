module Wrench.Filesystem(
  getFilesInDir
  ) where

import           ClassyPrelude
import           System.Directory (doesFileExist, getDirectoryContents)

getDirectoryContentsWrapped :: FilePath -> IO [FilePath]
getDirectoryContentsWrapped fp = (fpFromString <$>) <$> (getDirectoryContents (fpToString fp))

doesFileExistWrapped :: FilePath -> IO Bool
doesFileExistWrapped = doesFileExist . fpToString

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = getDirectoryContentsWrapped dir >>= (return . map (dir </>)) >>= filterM doesFileExistWrapped
