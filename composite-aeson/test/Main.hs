import Test.Hspec (hspec)

import DateTimeSpec (dateTimeSuite)
import EnumSpec (enumSuite)
import RecordSpec (recordSuite)
import THSpec (thSuite)
import TupleSpec (tupleSuite, namedTupleSuite)

main :: IO ()
main = hspec $ do
  dateTimeSuite
  enumSuite
  namedTupleSuite
  recordSuite
  thSuite
  tupleSuite
