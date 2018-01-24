module Types where

import ApiOrphans ()
import ClassyPrelude
import Control.Lens (ix, over)
import Control.Lens.TH (makeWrapped)
import Composite ((:->), Record)
import Composite.Aeson (DefaultJsonFormat(defaultJsonFormat), enumJsonFormat)
import Composite.Aeson.TH (makeRecordJsonWrapper)
import Composite.Opaleye (defaultRecTable)
import Composite.Opaleye.TH (deriveOpaleyeEnum)
import Composite.Swagger.TH (makeToSchema)
import Composite.TH (withLensesAndProxies)
import Data.Swagger
  ( ToParamSchema, ToSchema
  , declareNamedSchema, defaultSchemaOptions, constructorTagModifier
  , genericToParamSchema, paramSchemaToNamedSchema, toParamSchema )
import Data.Text (replace)
import Opaleye (Column, PGInt8, PGText, Table(Table))
import Web.HttpApiData (ToHttpApiData, FromHttpApiData, toUrlPiece, parseUrlPiece)

data UserType
  = UserTypeOwner
  | UserTypeManager
  | UserTypeRegular
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance DefaultJsonFormat UserType where
  defaultJsonFormat = enumJsonFormat "UserType"

deriveOpaleyeEnum ''UserType "usertype" (stripPrefix "UserType")

-- Manual swagger instances for sum type
instance ToParamSchema UserType where
  toParamSchema = genericToParamSchema $ unprefix "UserType"
    where
      unprefix prefix = defaultSchemaOptions
        { constructorTagModifier = unpack . over (ix 1) charToLower . replace prefix "" . pack }
instance ToSchema UserType where
  declareNamedSchema = pure . paramSchemaToNamedSchema defaultSchemaOptions

-- Dumb instances here - they don't map to opaleye enums or json formats
instance ToHttpApiData UserType where
  toUrlPiece typ = let x = tshow typ in fromMaybe x (stripPrefix "UserType" x)
instance FromHttpApiData UserType where
  parseUrlPiece = maybe (Left "could not parse") Right . readMay . ("UserType" <>)

withLensesAndProxies [d|
  type FId       = "id"       :-> Int64
  type CId       = "id"       :-> Column PGInt8
  type FIdMay    = "id"       :-> Maybe Int64
  type CIdMay    = "id"       :-> Maybe (Column PGInt8)
  type FLogin    = "login"    :-> Text
  type CLogin    = "login"    :-> Column PGText
  type FUserType = "usertype" :-> UserType
  type CUserType = "usertype" :-> Column PGUserType
  |]


type ApiUser       = '[FLogin, FUserType]
type DbUser        = '[FId, FLogin, FUserType]
type DbUserInsCols = '[CIdMay, CLogin, CUserType]
type DbUserCols    = '[CId, CLogin, CUserType]

userTable :: Table (Record DbUserInsCols) (Record DbUserCols)
userTable = Table "users" defaultRecTable

makeRecordJsonWrapper "ApiUserJson" ''ApiUser
makeWrapped ''ApiUserJson
makeToSchema "ApiUserJson" ''ApiUserJson
