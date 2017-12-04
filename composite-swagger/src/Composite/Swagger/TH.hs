module Composite.Swagger.TH where

import Composite.Swagger.Base (wrappedSchema)
import Composite.TH (makeRecordWrapper)
import Data.Proxy (Proxy (Proxy))
import Data.Swagger (ToSchema, declareNamedSchema)
import Language.Haskell.TH
  ( Dec, Name, Q, mkName
  , clause, conT, cxt, funD, instanceD, normalB, wildP )

makeToSchema :: String -> Name -> Q [Dec]
makeToSchema schemaName wrapperName =
  sequence
    [ instanceD
        (cxt [])
        [t| ToSchema $(conT wrapperName) |]
        [ funD
            'declareNamedSchema
            [ clause [wildP] (normalB [| wrappedSchema (Proxy :: Proxy $(conT wrapperName)) schemaName |]) [] ]
        ]
    ]

makeToSchemaWrapper :: String -> Name -> Q [Dec]
makeToSchemaWrapper wrapperNameStr fieldsTyName = do
  let wrapperName = mkName wrapperNameStr
  wrapperNewtype <- makeRecordWrapper wrapperNameStr fieldsTyName
  wrapperInstances <- makeToSchema wrapperNameStr wrapperName
  pure $ wrapperNewtype ++ wrapperInstances
