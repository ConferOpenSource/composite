module Types where

import ClassyPrelude hiding (Handler, Identity(Identity))
import Composite.Aeson
  ( DefaultJsonFormat(defaultJsonFormat), defaultJsonFormatRec
  , JsonFormat, recJsonFormat, enumJsonFormat
  , parseJsonWithFormat', toJsonWithFormat, jsonFormatWithIso
  )
import Composite.Opaleye (defaultRecTable)
import Composite.Opaleye.TH (deriveOpaleyeEnum)
import Composite.TH (withProxies)
import Control.Lens (_Wrapped)
import Control.Lens.TH (makeWrapped)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
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

withProxies [d|
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

newtype ApiUserJson = ApiUserJson { unApiUserJson :: Record ApiUser }
makeWrapped ''ApiUserJson

apiUserJsonFormat :: JsonFormat e ApiUserJson
apiUserJsonFormat = jsonFormatWithIso _Wrapped (recJsonFormat defaultJsonFormatRec)

instance ToJSON ApiUserJson where
  toJSON = toJsonWithFormat apiUserJsonFormat
instance FromJSON ApiUserJson where
  parseJSON = parseJsonWithFormat' apiUserJsonFormat
