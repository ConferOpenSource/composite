module Types where

import ClassyPrelude hiding (Handler, optional)
import Control.Lens.TH (makeWrapped)
import Composite ((:->), Record)
import Composite.Aeson (DefaultJsonFormat(defaultJsonFormat), enumJsonFormat)
import Composite.Aeson.TH (makeRecordJsonWrapper)
import Composite.Opaleye (defaultRecTable)
import Composite.Opaleye.TH (deriveOpaleyeEnum)
import Composite.TH (withLensesAndProxies)
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
instance ToHttpApiData UserType where
  toUrlPiece = tshow
instance FromHttpApiData UserType where
  parseUrlPiece = maybe (Left "could not parse") Right . readMay

withLensesAndProxies [d|
  type FId       = "id"       :-> Int64
  type CId       = "id"       :-> Column PGInt8
  type CIdMay    = "id"       :-> Maybe (Column PGInt8)
  type FLogin    = "login"    :-> Text
  type CLogin    = "login"    :-> Column PGText
  type FUserType = "usertype" :-> UserType
  type CUserType = "usertype" :-> Column PGUserType
  |]


type ApiUser       = '[FLogin, FUserType]
type DbUser        = '[FId, FLogin, FUserType]
type WriteColumns  = '[CIdMay, CLogin, CUserType]
type ReadColumns   = '[CId, CLogin, CUserType]

userTable :: Table (Record WriteColumns) (Record ReadColumns)
userTable = Table "users" defaultRecTable

makeRecordJsonWrapper "ApiUserJson" ''ApiUser
makeWrapped ''ApiUserJson
