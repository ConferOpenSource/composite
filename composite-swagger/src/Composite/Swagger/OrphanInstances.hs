{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Composite.Swagger.OrphanInstances where

import Composite ((:->), Record)
import Control.Lens ((%~), (&))
import qualified Data.HashMap.Strict.InsOrd as I
import Data.Proxy (Proxy (Proxy))
import Data.Swagger
  ( NamedSchema (NamedSchema), ToSchema
  , declareNamedSchema, declareSchemaRef, properties, schema )
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, symbolVal)

-- Orphan instances for 'Data.Vinyl.Record' that stuff a name/parameter schema into a
-- 'Data.Swagger.Schema' object.

instance ToSchema (Record '[]) where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance forall a s rs. (ToSchema a, ToSchema (Record rs), KnownSymbol s) => ToSchema (Record ((s :-> a) ': rs)) where
  declareNamedSchema _ = do
    xs <- declareNamedSchema (Proxy :: Proxy (Record rs))
    x <- declareSchemaRef (Proxy :: Proxy a)
    let name = Text.pack $ symbolVal (Proxy :: Proxy s)
    pure $ xs
      & schema.properties %~ I.insert name x
