module Composite.Aeson.Path where

import Composite.Aeson(JsonFormat, aesonJsonFormat)
import Path(Path, Rel, Abs, File, Dir)

relFileJsonFormat :: JsonFormat e (Path Rel File)
relFileJsonFormat = aesonJsonFormat

relDirJsonFormat :: JsonFormat e (Path Rel Dir)
relDirJsonFormat = aesonJsonFormat

absFileJsonFormat :: JsonFormat e (Path Abs File)
absFileJsonFormat = aesonJsonFormat

absDirJsonFormat :: JsonFormat e (Path Abs Dir)
absDirJsonFormat = aesonJsonFormat
