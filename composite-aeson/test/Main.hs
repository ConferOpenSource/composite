import Test.Hspec (hspec)

import EnumSpec (enumSuite)
import RecordSpec (recordSuite)
import THSpec (thSuite)
import TupleSpec (tupleSuite)

main :: IO ()
main = hspec $ do
  enumSuite
  recordSuite
  thSuite
  tupleSuite
