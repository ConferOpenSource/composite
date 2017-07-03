import Test.Hspec (hspec)

import UpdateSpec (updateSuite)

main :: IO ()
main = hspec $ do
  updateSuite
