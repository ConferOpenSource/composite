module FieldSpec where

import Composite (Field, (:->))
import Composite.Aeson.Base (JsonFormat, fromJsonWithFormat, toJsonWithFormat)
import Composite.Aeson.CoRecord (defaultJsonFormatField, fieldJsonFormat, optionalField)
import Composite.Aeson.Formats.Generic (SumStyle(SumStyleFieldName))
import Composite.TH (withPrismsAndProxies)
import Control.Lens (_Right, review)
import Data.Aeson.BetterErrors (parseValue)
import Data.Aeson.QQ (aesonQQ)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

withPrismsAndProxies [d|
  type FFoo = "foo" :-> Int
  type FBar = "bar" :-> Maybe String
  |]
type TestField = '["foo" :-> Int, "bar" :-> Maybe String]

fieldSuite :: Spec
fieldSuite =
  describe "Field support" $ do
    let defaultFmt :: JsonFormat Void (Field TestField)
        defaultFmt = fieldJsonFormat defaultJsonFormatField SumStyleFieldName

    it "works for encoding FFoo" $ do
      toJsonWithFormat defaultFmt (review _FFoo 123) `shouldBe` [aesonQQ| {foo: 123} |]
    it "works for encoding FBar" $ do
      toJsonWithFormat defaultFmt (review _FBar (Just "hi")) `shouldBe` [aesonQQ| {bar: "hi"} |]
    it "works for decoding FFoo" $ do
      parseValue (fromJsonWithFormat defaultFmt) [aesonQQ| {foo: 123} |] `shouldBe` review (_Right . _FFoo) 123
    it "works for decoding FBar" $ do
      parseValue (fromJsonWithFormat defaultFmt) [aesonQQ| {bar: "hi"} |] `shouldBe` review (_Right . _FBar) "hi"
