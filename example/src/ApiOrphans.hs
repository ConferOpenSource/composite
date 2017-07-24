{-# OPTIONS_GHC -fno-warn-orphans #-}
module ApiOrphans where

import Composite.Record ((:->)(Val))
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)

deriving instance ToHttpApiData a => ToHttpApiData (s :-> a)
deriving instance FromHttpApiData a => FromHttpApiData (s :-> a)
