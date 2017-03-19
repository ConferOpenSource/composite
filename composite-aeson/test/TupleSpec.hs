module TupleSpec where

import BasicPrelude
import Composite.Aeson.Base (JsonFormat, fromJsonWithFormat, toJsonWithFormat)
import Composite.Aeson.Formats.Provided (tuple3JsonFormat, integralJsonFormat, stringJsonFormat, charJsonFormat)
import Data.Aeson.BetterErrors (parseValue)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

tupleSuite :: Spec
tupleSuite =
  describe "Tuple formats" $ do
    it "works for 3-tuples" $ do
      let fmt = tuple3JsonFormat (integralJsonFormat :: JsonFormat Void Int) stringJsonFormat charJsonFormat
      property $ \ t ->
        parseValue (fromJsonWithFormat fmt) (toJsonWithFormat fmt t) `shouldBe` Right t
