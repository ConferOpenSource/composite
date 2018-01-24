import THSpec (thSuite)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  thSuite
