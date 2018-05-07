module TupleSpec where

import Composite.Aeson.Base (JsonFormat, fromJsonWithFormat, toJsonWithFormat)
import Composite.Aeson.Formats.Provided (tuple3JsonFormat, namedTuple3JsonFormat, integralJsonFormat, stringJsonFormat, charJsonFormat)
import Data.Aeson.BetterErrors (parseValue)
import Data.Aeson.QQ (aesonQQ)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

tupleSuite :: Spec
tupleSuite =
  describe "Tuple formats" $ do
    it "works for 3-tuples" $ do
      let fmt :: JsonFormat Void (Int, String, Char)
          fmt = tuple3JsonFormat integralJsonFormat stringJsonFormat charJsonFormat
      property $ \ t ->
        parseValue (fromJsonWithFormat fmt) (toJsonWithFormat fmt t) `shouldBe` Right t

namedTupleSuite :: Spec
namedTupleSuite =
  describe "Named tuple formats" $ do
    let fmt :: JsonFormat Void (String, String, String)
        fmt = namedTuple3JsonFormat "foo" stringJsonFormat "bar" stringJsonFormat "baz" stringJsonFormat

    it "round trips" $
      property $ \ t ->
        parseValue (fromJsonWithFormat fmt) (toJsonWithFormat fmt t) `shouldBe` Right t

    it "property names the fields" $
      toJsonWithFormat fmt ("a", "b", "c") `shouldBe` [aesonQQ| {foo: "a", bar: "b", baz: "c"} |]
