module Foundation
  ( AppStackM
  , AppData(AppData, appConnPool, appMetrics), withDb
  , module Metrics
  ) where

import ClassyPrelude hiding (Handler)
import Composite.Record (Record)
import Control.Monad.Logger (LoggingT)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Metrics
  ( configureMetrics, EkgMetrics, fActiveUsers, fResponseTimes
  , fUserCreateRequests, fUserRetrieveRequests, fUserUpdateRequests
  , fUserDeleteRequests, fUserEnumerateRequests )
import Servant (Handler)


-- |The context in which all requests will be evaluated to allow logging and accessing the application state
type AppStackM = ReaderT AppData (LoggingT Handler)

-- |The global application state
data AppData = AppData
  { appConnPool :: Pool Connection
  , appMetrics :: Record EkgMetrics
  }

withDb :: (MonadBaseControl IO m, MonadReader AppData m) => (Connection -> IO a) -> m a
withDb action = do
  pool <- asks appConnPool
  liftBase $ withResource pool action
