module Composite.Swagger.Base where

import Control.Lens (Unwrapped, Wrapped, (&), (.~))
import Composite.Swagger.OrphanInstances ()
import Data.Proxy (Proxy (Proxy))
import Data.Swagger
  ( Definitions, NamedSchema(NamedSchema), Schema, SwaggerType(SwaggerObject), ToSchema
  , declareSchema, type_ )
import Data.Swagger.Declare (Declare)
import qualified Data.Text as Text

-- |Given a 'Control.Lens.Wrapped' and an underlying 'Data.Swagger.ToSchema' instance, create a
-- Schema with the given name surrounding the underlying instance.
wrappedSchema :: (Wrapped wrap, ToSchema (Unwrapped wrap)) => Proxy wrap -> String -> Declare (Definitions Schema) NamedSchema
wrappedSchema (Proxy :: Proxy wrap) name = do
  s <- declareSchema (Proxy :: Proxy (Unwrapped wrap))
  pure $ NamedSchema (Just $ Text.pack name) s
    & type_ .~ SwaggerObject
