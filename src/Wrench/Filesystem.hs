module Wrench.Filesystem(
  getFilesInDir
  ) where

import           ClassyPrelude hiding(FilePath,(</>))
import           System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath

getDirectoryContentsWrapped :: FilePath -> IO [FilePath]
getDirectoryContentsWrapped fp = getDirectoryContents fp

doesFileExistWrapped :: FilePath -> IO Bool
doesFileExistWrapped = doesFileExist

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = getDirectoryContentsWrapped dir >>= (return . map (dir </>)) >>= filterM doesFileExistWrapped
