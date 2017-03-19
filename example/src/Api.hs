module Api (API, api, service) where

import ClassyPrelude
import Control.Arrow (returnA)
import Control.Lens (_Unwrapping, each, toListOf, view)
import Control.Monad.Logger (logInfo)
import Data.Proxy (Proxy(Proxy))
import Foundation (AppStackM, appMetrics, withDb, fUserRequests)
import Frames (Record, rsubset)
import Opaleye ((./=), constant, desc, orderBy, queryTable, restrict, runQuery)
import Servant (ServerT, Get, JSON, (:>))
import Types (ApiUserJson(ApiUserJson), DbUser, UserType(UserTypeRegular), cLogin, cUserType, userTable)
import qualified System.Metrics.Counter as Counter

type API = "users" :> Get '[JSON] [ApiUserJson]

api :: Proxy API
api = Proxy

service :: ServerT API AppStackM
service = do
  $logInfo "received request for users"
  liftIO . Counter.inc =<< asks (view fUserRequests . appMetrics) -- Increment the user requests ekg counter

  users <- withDb $ \ conn ->
    runQuery conn . orderBy (desc $ view cLogin) $ proc () -> do
      user@(view cUserType -> userType) <- queryTable userTable -< ()
      restrict -< userType ./= constant UserTypeRegular
      returnA -< user

  let _ = users :: [Record DbUser]

  pure $ toListOf (each . rsubset . _Unwrapping ApiUserJson) users
