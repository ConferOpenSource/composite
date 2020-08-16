module Composite.Aeson.Path where

import Composite.Aeson(JsonFormat, aesonJsonFormat, DefaultJsonFormat(defaultJsonFormat))
import Path(Path, Rel, Abs, File, Dir)

relFileJsonFormat :: JsonFormat e (Path Rel File)
relFileJsonFormat = aesonJsonFormat

relDirJsonFormat :: JsonFormat e (Path Rel Dir)
relDirJsonFormat = aesonJsonFormat

absFileJsonFormat :: JsonFormat e (Path Abs File)
absFileJsonFormat = aesonJsonFormat

absDirJsonFormat :: JsonFormat e (Path Abs Dir)
absDirJsonFormat = aesonJsonFormat

instance DefaultJsonFormat (Path Rel File) where
  defaultJsonFormat = relFileJsonFormat

instance DefaultJsonFormat (Path Rel Dir) where
  defaultJsonFormat = relDirJsonFormat

instance DefaultJsonFormat (Path Abs File) where
  defaultJsonFormat = absFileJsonFormat

instance DefaultJsonFormat (Path Abs Dir) where
  defaultJsonFormat = absDirJsonFormat
