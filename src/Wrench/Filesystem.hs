module Wrench.Filesystem(
  getFilesInDir
  ) where

import           Control.Monad    (filterM, return, (>>=))
import           Data.Function    ((.))
import           Data.List        (map)
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath
import           System.IO        (IO)

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = getDirectoryContents dir >>= (return . map (dir </>)) >>= filterM doesFileExist
