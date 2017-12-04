module THSpec where

import Composite
import Composite.Aeson.TH (makeRecordJsonWrapper)
import Composite.Swagger.TH (makeToSchema)
import Composite.TH (withLensesAndProxies)
import Control.Lens.TH (makeWrapped)
import Data.Swagger (validateToJSON)
import Test.Hspec (Spec, describe, it, shouldBe)

withLensesAndProxies [d|
  type FFoo = "foo" :-> Int
  type FBar = "bar" :-> String
  |]
type TestRec = '["foo" :-> Int, "bar" :-> String]

makeRecordJsonWrapper "TestRecJson" ''TestRec
makeWrapped ''TestRecJson
makeToSchema "TestRecJson" ''TestRecJson

thSuite :: Spec
thSuite = do
  describe "Swagger Record Support" $ do
    it "encodes all fields" $ do
      let x = TestRecJson $ 1 :*: "a" :*: RNil
      validateToJSON x `shouldBe` mempty
