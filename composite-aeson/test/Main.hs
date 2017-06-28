import RecordSpec (recordSuite)
import TupleSpec (tupleSuite)
import EnumSpec (enumSuite)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  recordSuite
  tupleSuite
  enumSuite
