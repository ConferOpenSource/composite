module Composite.Swagger.Base where

import Control.Lens (Unwrapped, Wrapped, (&), (.~))
import Composite.Swagger.OrphanInstances ()
import Data.Proxy (Proxy (Proxy))
import Data.Swagger
  ( Definitions, NamedSchema(NamedSchema), Schema, SwaggerType(SwaggerObject), ToSchema
  , declareSchema, type_ )
import Data.Swagger.Declare (Declare)
import qualified Data.Text as Text

wrappedSchema :: (Wrapped wrap, ToSchema (Unwrapped wrap)) => Proxy wrap -> String -> Declare (Definitions Schema) NamedSchema
wrappedSchema (Proxy :: Proxy wrap) name = do
  s <- declareSchema (Proxy :: Proxy (Unwrapped wrap))
  pure $ NamedSchema (Just $ Text.pack name) s
    & type_ .~ SwaggerObject
