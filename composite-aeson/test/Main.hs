import BasicPrelude
import RecordSpec (recordSuite)
import TupleSpec (tupleSuite)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  recordSuite
  tupleSuite

