module DateTimeSpec where

import Composite.Aeson (parseJsonWithFormat', toJsonWithFormat)
import Composite.Aeson.Formats.DateTime (iso8601DateTimeJsonFormat)
import Data.Aeson.Types (parseEither)
import Data.Aeson.QQ (aesonQQ)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Clock (UTCTime(UTCTime))
import Test.Hspec (Spec, describe, it, shouldBe)


testDate :: UTCTime
testDate = UTCTime (ModifiedJulianDay 58025) (fromRational 72151.986)

dateTimeSuite :: Spec
dateTimeSuite = do
  describe "Composite.Aeson.Formats.DateTime" $ do
    it "encodes ISO8601 date time format for a test date" $
      toJsonWithFormat iso8601DateTimeJsonFormat testDate `shouldBe` [aesonQQ|"2017-09-29T20:02:31.986+0000"|]
    it "decodes ISO8601 date time format for a test date" $
      parseEither (parseJsonWithFormat' iso8601DateTimeJsonFormat) [aesonQQ|"2017-09-29T20:02:31.986+0000"|] `shouldBe` Right testDate
