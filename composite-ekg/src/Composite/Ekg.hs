module Composite.Ekg (EkgMetric(ekgMetric)) where

import BasicPrelude
import Composite.Record ((:->)(Val), Rec((:&), RNil), Record)
import Data.Char (isUpper, toLower)
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Text as Text
import Data.Proxy (Proxy(Proxy))
import Data.Text (pack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.Metrics (Store, createCounter, createGauge, createLabel, createDistribution)
import System.Metrics.Counter (Counter)
import System.Metrics.Gauge (Gauge)
import System.Metrics.Label (Label)
import System.Metrics.Distribution (Distribution)

-- |Type class for constructing a configured EKG metric store for record type of named fields
--
-- For example, given:
--
-- > type FActiveUsers    = "activeUsers"           :-> Gauge
-- > type FResponseTimes  = "endpointResponseTimes" :-> Distribution
-- > type FOrdersPlaced   = "ordersPlaced"          :-> Counter
-- > type EkgMetrics = '[FActiveUsers, FResponseTimes, FRevenue]
--
-- And then used in:
--
-- > configureMetrics :: IO (Rec EkgMetrics)
-- > configureMetrics = do
-- >   store <- newStore
-- >   metrics <- ekgMetric "myapp" store
-- >   _ <- forkServerWith store "localhost" 8080
-- >   pure metrics
--
-- Compare to a more traditional:
--
-- > metrics <- EkgMetrics
-- >  <$> createGauge "myapp.active_users store
-- >  <*> createDistribution "myapp.endpoint_response_times" store
-- >  <*> createCounter "myapp.orders_placed" store
--
-- The former is more concise and harder to make naming errors particularly in larger store sets
class EkgMetric a where
  ekgMetric :: Text -> Store -> IO a

instance forall a s rs. (EkgMetric a, EkgMetric (Record rs), KnownSymbol s) => EkgMetric (Record ((s :-> a) ': rs)) where
  ekgMetric prefix store =
    (:&)
      <$> (Identity . Val <$> ekgMetric (prefix <> "." <> (upperScores . pack . symbolVal) (Proxy :: Proxy s)) store)
      <*> ekgMetric prefix store

instance EkgMetric (Record '[]) where
  ekgMetric _ _ = pure RNil

instance EkgMetric Counter where
  ekgMetric = createCounter

instance EkgMetric Gauge where
  ekgMetric = createGauge

instance EkgMetric Label where
  ekgMetric = createLabel

instance EkgMetric Distribution where
  ekgMetric = createDistribution

upperScores :: Text -> Text
upperScores = Text.dropWhile (== '_') . Text.concatMap score
  where score :: Char -> Text
        score c | isUpper c = "_" <> Text.singleton (toLower c)
        score c = Text.singleton c
