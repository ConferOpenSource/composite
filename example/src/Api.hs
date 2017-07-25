module Api where

import ClassyPrelude
import Composite.Record (Record, (:->)(Val), pattern (:*:), getVal)
import Control.Arrow (returnA)
import Control.Lens (_Unwrapping, each, toListOf, view)
import Control.Monad.Except (throwError)
import Control.Monad.Logger (logInfo)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl.Lens (rsubset)
import Foundation
  ( AppStackM, appMetrics, withDb
  , fUserCreateRequests, fUserRetrieveRequests, fUserUpdateRequests
  , fUserDeleteRequests, fUserEnumerateRequests )
import Opaleye
  ( (.==), (.&&), constant, desc, limit, orderBy, queryTable, restrict
  , runDelete, runInsertMany, runQuery, runUpdate )
import Servant (Capture, Delete, Get, JSON, Post, Put, QueryParam, ReqBody, (:>), (:<|>))
import Servant.Server (err404)
import Types
  ( ApiUser, ApiUserJson(ApiUserJson), DbUser, FId, FIdMay, FLogin, FUserType, DbUserInsCols
  , cId, cLogin, cUserType, userTable )
import qualified System.Metrics.Counter as Counter

type API = "users" :> ( ReqBody '[JSON] ApiUserJson :> Post '[JSON] ()
                        :<|> Capture "userKey" FId :> Get '[JSON] ApiUserJson
                        :<|> Capture "userKey" FId :> ReqBody '[JSON] ApiUserJson :> Put '[JSON] ()
                        :<|> Capture "userKey" FId :> Delete '[JSON] ()
                        :<|> QueryParam "login" FLogin :> QueryParam "type" FUserType :> Get '[JSON] [ApiUserJson]
                      )

api :: Proxy API
api = Proxy

-- |Convert an id-less User to something that may have a key if it's provided on input; needed
-- because @Opaleye.Table.TableProperties@ uses writer columns for inserts and deletes
toWrite :: Maybe FId -> Record ApiUser -> Record DbUserInsCols
toWrite userKeyMay user =
  let convert = constant :: Record (FIdMay ': ApiUser) -> Record DbUserInsCols
  in convert $ case userKeyMay of
    Just (Val userKey) -> Just userKey :*: user
    Nothing -> Nothing :*: user

-- |Create a user from some fields
createUser :: ApiUserJson -> AppStackM ()
createUser (ApiUserJson user) = do
  $logInfo "received create request"
  -- Increment the user create requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserCreateRequests . appMetrics)

  void $ withDb $ \ conn -> runInsertMany conn userTable [toWrite Nothing user]

-- |Retrieve a user by key
retrieveUser :: FId -> AppStackM ApiUserJson
retrieveUser (Val userKey) = do
  $logInfo "received retrieve request"
  -- Increment the user retrieve requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserRetrieveRequests . appMetrics)

  users <- withDb $ \ conn ->
    runQuery conn . limit 1 $ proc () -> do
      user <- queryTable userTable -< ()
      restrict -< view cId user .== constant userKey
      returnA -< user

  let _ = users :: [Record DbUser]
  case headMay users of
    Just user -> pure $ view (rsubset . _Unwrapping ApiUserJson) user
    Nothing -> throwError err404

-- |Replace a user by key
updateUser :: FId -> ApiUserJson -> AppStackM ()
updateUser uId@(Val userKey) (ApiUserJson user) = do
  $logInfo "received update request"
  -- Increment the user update requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserUpdateRequests . appMetrics)

  void $ withDb $ \ conn -> runUpdate conn userTable (const $ toWrite (Just uId) user) $
    \ u -> view cId u .== constant userKey

-- |Delete a user by key
deleteUser :: FId -> AppStackM ()
deleteUser (Val userKey) = do
  $logInfo "received delete request"
  -- Increment the user delete requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserDeleteRequests . appMetrics)

  void $ withDb $ \ conn -> runDelete conn userTable $
    \ u -> view cId u .== constant userKey

-- |List users - omitting query parameters results in unbounded query
enumerateUsers :: Maybe FLogin -> Maybe FUserType -> AppStackM [ApiUserJson]
enumerateUsers login userType = do
  $logInfo "received enumerate request"
  -- Increment the user enumerate requests ekg counter
  liftIO . Counter.inc =<< asks (view fUserEnumerateRequests . appMetrics)

  users <- withDb $ \ conn -> runQuery conn . orderBy (desc $ view cLogin) $ proc () -> do
    user <- queryTable userTable -< ()
    restrict -< maybe (constant True) ((.== view cUserType user) . constant . getVal) userType
      .&& maybe (constant True) ((.== view cLogin user) . constant . getVal) login
    returnA -< user

  let _ = users :: [Record DbUser]
  pure $ toListOf (each . rsubset . _Unwrapping ApiUserJson) users
