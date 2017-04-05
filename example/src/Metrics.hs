module Metrics where

import ClassyPrelude
import Composite.Ekg (ekgMetric)
import Composite.Record ((:->), Record)
import Composite.TH (withLensesAndProxies)
import qualified System.Metrics as EKG
import System.Metrics.Counter (Counter)
import System.Metrics.Gauge (Gauge)
import System.Metrics.Distribution (Distribution)
import qualified System.Remote.Monitoring as EKG

withLensesAndProxies [d|
  type FActiveUsers    = "activeUsers"           :-> Gauge
  type FResponseTimes  = "endpointResponseTimes" :-> Distribution
  type FUserRequests   = "userRequests"          :-> Counter
  |]
type EkgMetrics = '[FActiveUsers, FResponseTimes, FUserRequests]

-- |Create a new ekg store and metrics record, register the relevant metrics, and start the ekg server
configureMetrics :: IO (Record EkgMetrics)
configureMetrics = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  metrics <- ekgMetric "myawesomeapp" store
  _ <- EKG.forkServerWith store "localhost" 8090
  pure metrics
