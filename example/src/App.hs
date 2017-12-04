module App (startApp) where

import ClassyPrelude hiding (Handler)

import Api
  ( swaggerApiDefinition, redirect, swaggerApi
  , createUser, retrieveUser, updateUser, deleteUser, enumerateUsers )
import Control.Monad.Logger (askLoggerIO, logInfo)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PG
import Foundation (AppData(AppData), AppStackM, configureMetrics)
import Logging (LogFunction, withLogger, withLoggingFunc)
import Network.Wai.Handler.Warp (run)
import Servant ((:<|>)((:<|>)), Handler, (:~>)(NT), enter, serve)
import Servant.Swagger.UI (swaggerSchemaUIServer)

-- |Perform app initialization and then begin serving the API
startApp :: IO ()
startApp = do
  withLogger $ do
    metrics <- liftIO configureMetrics
    withPostgresqlPool "host=localhost port=5432 user=example dbname=example" 5 $ \ connPool -> do
      let appData = AppData connPool metrics
      logFn <- askLoggerIO
      $logInfo $ "Starting server on port 8080"
      liftIO . run 8080 . serve swaggerApi
        $ enter (appStackToHandler appData logFn) ( createUser :<|> retrieveUser :<|> updateUser :<|> deleteUser :<|> enumerateUsers )
          :<|> swaggerSchemaUIServer swaggerApiDefinition -- serve the Swagger docs
          :<|> redirect "/dev/index.html" -- redirect to the Swagger docs at '/'

withPostgresqlPool :: MonadBaseControl IO m => ByteString -> Int -> (Pool PG.Connection -> m a) -> m a
withPostgresqlPool connStr nConns action = do
  stm <- liftBaseWith $ \ runInBase ->
    bracket createPool Pool.destroyAllResources (runInBase . action)
  restoreM stm
  where
    createPool = Pool.createPool (PG.connectPostgreSQL connStr) PG.close 1 20 nConns

appStackToHandler' :: forall a. AppData -> LogFunction -> AppStackM a -> Handler a
appStackToHandler' appData logger action = withLoggingFunc logger $ runReaderT action appData

appStackToHandler :: AppData -> LogFunction -> (AppStackM :~> Handler)
appStackToHandler ad lf = NT $ appStackToHandler' ad lf
