import BasicPrelude
import RecordSpec (recordSuite)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  recordSuite

