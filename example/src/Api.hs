module Api where

import ClassyPrelude
import Composite.Record (pattern (:*:), (:->) (Val), Record, getVal)
import Control.Arrow (returnA)
import Control.Lens (each, toListOf, view, (&), (.~), (?~), _Unwrapping)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Logger (logInfo)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.HashMap.Strict.InsOrd as I
import Data.Proxy (Proxy (Proxy))
import Data.Swagger
  ( NamedSchema (NamedSchema), Referenced (Inline), Swagger, SwaggerType (SwaggerObject, SwaggerString), ToSchema
  , declareNamedSchema, description, enum_, info, license, properties, title, type_, version )
import Data.Version (showVersion)
import Data.Vinyl.Lens (rsubset)
import Foundation
  ( AppStackM, appMetrics, withDb
  , fUserCreateRequests, fUserDeleteRequests, fUserEnumerateRequests, fUserRetrieveRequests, fUserUpdateRequests )
import Opaleye
  ( constant, desc, limit, orderBy, queryTable, restrict
  , runDelete, runInsertMany, runQuery, runUpdate
  , (.&&), (.==) )
import qualified Paths_myawesomeserver
import Servant ((:<|>), (:>), Capture, Delete, Get, JSON, Post, Put, QueryParam, ReqBody)
import Servant.Server (ServerError, err307, err404, errHeaders)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI)
import qualified System.Metrics.Counter as Counter
import Types
  ( ApiUser, ApiUserJson (ApiUserJson), DbUser, DbUserInsCols, FId, FIdMay, FLogin, FUserType
  , cId, cLogin, cUserType, userTable )

-- |A catastrophically simple type for API responses, definitely don't use this in real code
data Result = Result
instance ToJSON Result where
  toJSON _ = object [ "result" .= asText "accepted" ]
instance ToSchema Result where
  declareNamedSchema _ =
    let accepted = Inline $ mempty & type_ ?~ SwaggerString & enum_ ?~ ["accepted"]
        inner = mempty & type_ ?~ SwaggerObject & properties .~ I.singleton "result" accepted
    in pure $ NamedSchema (Just "Result") inner

type API = "users" :> ( ReqBody '[JSON] ApiUserJson :> Post '[JSON] Result
                        :<|> Capture "userKey" FId :> Get '[JSON] ApiUserJson
                        :<|> Capture "userKey" FId :> ReqBody '[JSON] ApiUserJson :> Put '[JSON] Result
                        :<|> Capture "userKey" FId :> Delete '[JSON] Result
                        :<|> QueryParam "login" FLogin :> QueryParam "type" FUserType :> Get '[JSON] [ApiUserJson]
                      )

-- |Routing for an API along with its Swagger documentation and a redirect from root to the docs.
type DocumentedApi api docsDir =
  api :<|>
  SwaggerSchemaUI docsDir "swagger.json" :<|>
  Get '[JSON] ()

-- |Always redirect to the given location.
redirect :: (MonadError ServerError m) => Text -> m a
redirect loc = throwError $ err307 { errHeaders = [("Location", fromString $ unpack loc)] }

api :: Proxy API
api = Proxy

swaggerApi :: Proxy (DocumentedApi API "dev")
swaggerApi = Proxy

swaggerApiDefinition :: Swagger
swaggerApiDefinition =
  toSwagger api
    & info.title .~ "Example API"
    & info.version .~ pack (showVersion Paths_myawesomeserver.version)
    & info.description ?~ "An example API"
    & info.license ?~ "BSD3"

-- |Convert an id-less User to something that may have a key if it's provided on input; needed
-- because @Opaleye.Table.TableProperties@ uses writer columns for inserts and deletes
toWrite :: Maybe FId -> Record ApiUser -> Record DbUserInsCols
toWrite userKeyMay user =
  let convert = constant :: Record (FIdMay ': ApiUser) -> Record DbUserInsCols
  in convert $ case userKeyMay of
    Just (Val userKey) -> Just userKey :*: user
    Nothing            -> Nothing :*: user

-- |Create a user from some fields
createUser :: ApiUserJson -> AppStackM Result
createUser (ApiUserJson user) = do
  $logInfo "received create request"
  -- Increment the user create requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserCreateRequests . appMetrics)

  void $ withDb $ \ conn -> liftIO $ runInsertMany conn userTable [toWrite Nothing user]
  pure Result

-- |Retrieve a user by key
retrieveUser :: FId -> AppStackM ApiUserJson
retrieveUser (Val userKey) = do
  $logInfo "received retrieve request"
  -- Increment the user retrieve requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserRetrieveRequests . appMetrics)

  users <- withDb $ \ conn -> liftIO $ 
    runQuery conn . limit 1 $ proc () -> do
      user <- queryTable userTable -< ()
      restrict -< view cId user .== constant userKey
      returnA -< user

  let _ = users :: [Record DbUser]
  case headMay users of
    Just user -> pure $ view (rsubset . _Unwrapping ApiUserJson) user
    Nothing   -> throwError err404

-- |Replace a user by key
updateUser :: FId -> ApiUserJson -> AppStackM Result
updateUser uId@(Val userKey) (ApiUserJson user) = do
  $logInfo "received update request"
  -- Increment the user update requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserUpdateRequests . appMetrics)

  void $ withDb $ \ conn -> liftIO $ runUpdate conn userTable (const $ toWrite (Just uId) user) $
    \ u -> view cId u .== constant userKey
  pure Result

-- |Delete a user by key
deleteUser :: FId -> AppStackM Result
deleteUser (Val userKey) = do
  $logInfo "received delete request"
  -- Increment the user delete requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserDeleteRequests . appMetrics)

  void $ withDb $ \ conn -> liftIO $ runDelete conn userTable $
    \ u -> view cId u .== constant userKey
  pure Result

-- |List users - omitting query parameters results in unbounded query
enumerateUsers :: Maybe FLogin -> Maybe FUserType -> AppStackM [ApiUserJson]
enumerateUsers login userType = do
  $logInfo "received enumerate request"
  -- Increment the user enumerate requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserEnumerateRequests . appMetrics)

  users <- withDb $ \ conn -> liftIO $ runQuery conn . orderBy (desc $ view cLogin) $ proc () -> do
    user <- queryTable userTable -< ()
    restrict -< maybe (constant True) ((.== view cUserType user) . constant . getVal) userType
      .&& maybe (constant True) ((.== view cLogin user) . constant . getVal) login
    returnA -< user

  let _ = users :: [Record DbUser]
  pure $ toListOf (each . rsubset . _Unwrapping ApiUserJson) users
