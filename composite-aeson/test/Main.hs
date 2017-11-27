import Test.Hspec (hspec)

import DateTimeSpec (dateTimeSuite)
import EnumSpec (enumSuite)
import RecordSpec (recordSuite)
import THSpec (thSuite)
import TupleSpec (tupleSuite)

main :: IO ()
main = hspec $ do
  dateTimeSuite
  enumSuite
  recordSuite
  thSuite
  tupleSuite
