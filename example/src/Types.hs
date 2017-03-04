module Types where

import ClassyPrelude hiding (Handler, Identity(Identity))
import Composite.Aeson
  ( DefaultJsonFormat(defaultJsonFormat), defaultJsonFormatRec
  , JsonFormat, recJsonFormat, enumJsonFormat
  , parseJsonWithFormat', toJsonWithFormat, jsonFormatWithIso
  )
import Composite.Opaleye (defaultRecTable)
import Control.Lens (_Wrapped)
import Control.Lens.TH (makeWrapped)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import qualified Data.ByteString.Char8 as BSC8
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default, def)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Simple (ResultError(ConversionFailed, Incompatible, UnexpectedNull))
import Database.PostgreSQL.Simple.FromField (FromField, fromField, typename, returnError)
import Frames ((:->), Record)
import Opaleye
  ( Column, Constant, QueryRunnerColumnDefault, PGInt8, PGText, Table(Table), fieldQueryRunnerColumn, queryRunnerColumnDefault
  , unsafeCoerceColumn
  )

data UserType
  = UserTypeOwner
  | UserTypeManager
  | UserTypeRegular
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance DefaultJsonFormat UserType where
  defaultJsonFormat = enumJsonFormat "UserType"

data PGUserType

type FId       = "id"       :-> Int64
type CId       = "id"       :-> Column PGInt8
type FLogin    = "login"    :-> Text
type CLogin    = "login"    :-> Column PGText
type FUserType = "usertype" :-> UserType
type CUserType = "usertype" :-> Column PGUserType

fId :: Proxy FId
fId = Proxy

cId :: Proxy CId
cId = Proxy

fLogin :: Proxy FLogin
fLogin = Proxy

cLogin :: Proxy CLogin
cLogin = Proxy

fUserType :: Proxy FUserType
fUserType = Proxy

cUserType :: Proxy CUserType
cUserType = Proxy

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


instance FromField UserType where
  fromField f mbs = do
    tname <- typename f
    case mbs of
      _ | tname /= "usertype" -> returnError Incompatible f ""
      Just "Owner"    -> pure UserTypeOwner
      Just "Manager"  -> pure UserTypeManager
      Just "Regular"  -> pure UserTypeRegular
      Just other      -> returnError ConversionFailed f ("Unexpected user type: " <> BSC8.unpack other)
      Nothing         -> returnError UnexpectedNull f ""

instance QueryRunnerColumnDefault PGUserType UserType where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

constantColumnUsing :: Constant haskell (Column pgType)
                    -> (haskell' -> haskell)
                    -> Constant haskell' (Column pgType')
constantColumnUsing oldConstant f = dimap f unsafeCoerceColumn oldConstant

instance Default Constant UserType (Column PGUserType) where
  def = constantColumnUsing (def :: Constant String (Column PGText)) $ \ case
    UserTypeOwner   -> "Owner"
    UserTypeManager -> "Manager"
    UserTypeRegular -> "Regular"

