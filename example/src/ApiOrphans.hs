{-# OPTIONS_GHC -fno-warn-orphans #-}
module ApiOrphans where

import Composite.Record ((:->)(Val))
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)

-- Orphan instances for using `s :-> a` as a @Servant.Capture@ or @Servant.QueryParam@
deriving instance ToHttpApiData a => ToHttpApiData (s :-> a)
deriving instance FromHttpApiData a => FromHttpApiData (s :-> a)
