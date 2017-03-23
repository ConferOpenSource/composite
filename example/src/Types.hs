module Types where

import ClassyPrelude hiding (Handler, Identity(Identity))
import Control.Lens.TH (makeWrapped)
import Composite.Aeson (DefaultJsonFormat(defaultJsonFormat), enumJsonFormat)
import Composite.Aeson.TH (makeRecJsonWrapper)
import Composite.Opaleye (defaultRecTable)
import Composite.Opaleye.TH (deriveOpaleyeEnum)
import Composite.TH (withLensesAndProxies)
import Frames ((:->), Record)
import Opaleye (Column, PGInt8, PGText, Table(Table))

data UserType
  = UserTypeOwner
  | UserTypeManager
  | UserTypeRegular
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance DefaultJsonFormat UserType where
  defaultJsonFormat = enumJsonFormat "UserType"

deriveOpaleyeEnum ''UserType "usertype" (stripPrefix "UserType")

withLensesAndProxies [d|
  type FId       = "id"       :-> Int64
  type CId       = "id"       :-> Column PGInt8
  type FLogin    = "login"    :-> Text
  type CLogin    = "login"    :-> Column PGText
  type FUserType = "usertype" :-> UserType
  type CUserType = "usertype" :-> Column PGUserType
  |]

type ApiUser       = '[FLogin, FUserType]
type DbUserColumns = '[CId, CLogin, CUserType]
type DbUser        = '[FId, FLogin, FUserType]

userTable :: Table (Record DbUserColumns) (Record DbUserColumns)
userTable = Table "users" defaultRecTable

makeRecJsonWrapper "ApiUserJson" ''ApiUser
makeWrapped ''ApiUserJson
