module Composite.Swagger.TH where

import Composite.CoRecord (Field)
import Composite.Swagger.Base (wrappedSchema)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Swagger (ToSchema, declareNamedSchema)
import Language.Haskell.TH
  ( Dec, Name, Q, mkName
  , bang, bangType, clause, conT, cxt, funD, instanceD, newtypeD, normalB, recC, varBangType , wildP
  , noSourceStrictness, noSourceUnpackedness )

-- |TH splice which makes it more convenient to define 'ToSchema' instance for 'Record' types.
--
-- For example:
--
-- @
--   type MyRecord = '[FFoo, FBar]
--   newtype MyRecordWrapper = MyRecordWrapper { unMyRecordWrapper :: Record MyRecord }
--   makeToSchema "MyRecordWrapper" ''MyRecordWrapper
-- @
--
-- is equivalent to:
--
-- @
--   instance ToSchema MyRecordWrapper where
--     declareNamedSchema = wrappedSchema (Proxy :: Proxy MyRecordWrapper) "MyRecordWrapper"
-- @
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

-- |TH splice which makes it more convenient to define 'ToSchema' instance for 'Record' types.
--
-- For example:
--
-- @
--   type MyRecord = '[FFoo, FBar]
--   makeToSchemaWrapper "MyRecordWrapper" ''MyRecord
-- @
--
-- is equivalent to:
--
-- @
--   newtype MyRecordWrapper = MyRecordWrapper { unMyRecordWrapper :: Record MyRecord }
--   instance ToSchema MyRecordWrapper where
--     declareNamedSchema = wrappedSchema (Proxy :: Proxy MyRecordWrapper) "MyRecordWrapper"
-- @
makeToSchemaWrapper :: String -> Name -> Q [Dec]
makeToSchemaWrapper wrapperNameStr fieldsTyName = do
  let wrapperName = mkName wrapperNameStr
      extractorName = mkName $ "un" <> wrapperNameStr
      fieldTy = [t| Field $(conT fieldsTyName) |]
  wrapperNewtype <- newtypeD
    (cxt [])
    wrapperName
    [] -- TyVarBndrs
    Nothing -- kind
    (recC wrapperName [varBangType extractorName (bangType (bang noSourceUnpackedness noSourceStrictness) fieldTy)])
    [] -- deriving context
  wrapperInstances <- makeToSchema wrapperNameStr wrapperName
  pure $ wrapperNewtype:wrapperInstances
