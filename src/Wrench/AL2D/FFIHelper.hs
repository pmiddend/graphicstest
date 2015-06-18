module Wrench.AL2D.FFIHelper where

import ClassyPrelude
import Foreign.C.String(withCString,CString)

withTextToCString :: Text -> (CString -> IO a) -> IO a
withTextToCString s a = withCString (unpack s) a
