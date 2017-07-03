module THSpec where

import Composite.Aeson.Base (dimapJsonFormat, fromJsonWithFormat, toJsonWithFormat)
import Composite.Aeson.CoRecord (JsonFormatField)
import Composite.Aeson.Formats.Default (defaultJsonFormat)
import Composite.Aeson.Formats.Generic (SumStyle(SumStyleFieldName))
import Composite.Aeson.Formats.Provided (integralJsonFormat)
import Composite.Aeson.Record (JsonFormatRecord, field)
import Composite.Aeson.TH (makeFieldJsonWrapper, makeFieldJsonWrapperExplicit, makeRecordJsonWrapper, makeRecordJsonWrapperExplicit)
import Composite.Record ((:->), Rec((:&), RNil), pattern (:*:))
import Composite.TH (withOpticsAndProxies)
import Control.Lens (review)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.BetterErrors  as ABE
import Data.Aeson.QQ (aesonQQ)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe)

withOpticsAndProxies [d|
  type FFoo = "foo" :-> Int
  |]

type Foo = '[FFoo]

explicitFooJsonFormatRecord :: JsonFormatRecord e Foo
explicitFooJsonFormatRecord
  =  field (dimapJsonFormat (+10) (subtract 10) integralJsonFormat)
  :& RNil

explicitFooJsonFormatField :: JsonFormatField e Foo
explicitFooJsonFormatField
  =  dimapJsonFormat (+10) (subtract 10) integralJsonFormat
  :& RNil

makeRecordJsonWrapper "FooRecordDefaultJson" ''Foo
makeRecordJsonWrapperExplicit "FooRecordExplicitJson" ''Foo [| explicitFooJsonFormatRecord |]
makeFieldJsonWrapper "FooFieldDefaultJson" ''Foo SumStyleFieldName
makeFieldJsonWrapperExplicit "FooFieldExplicitJson" ''Foo SumStyleFieldName [| explicitFooJsonFormatField |]

deriving instance Eq FooRecordDefaultJson
deriving instance Eq FooRecordExplicitJson
deriving instance Eq FooFieldDefaultJson
deriving instance Eq FooFieldExplicitJson

deriving instance Show FooRecordDefaultJson
deriving instance Show FooRecordExplicitJson
deriving instance Show FooFieldDefaultJson
deriving instance Show FooFieldExplicitJson

thSuite :: Spec
thSuite = do
  describe "Composite.Aeson.TH" $ do
    it "decodes via FromJSON using makeRecordJsonWrapper" $ do
      Aeson.parseEither Aeson.parseJSON [aesonQQ| {foo: 123} |] `shouldBe` Right (FooRecordDefaultJson $ 123 :*: RNil)
    it "encodes via ToJSON using makeRecordJsonWrapper" $ do
      Aeson.toJSON (FooRecordDefaultJson $ 123 :*: RNil) `shouldBe` [aesonQQ| {foo: 123} |]
    it "decodes via DefaultJsonFormat using makeRecordJsonWrapper" $ do
      ABE.parseValue @Void (fromJsonWithFormat defaultJsonFormat) [aesonQQ| {foo: 123} |] `shouldBe` Right (FooRecordDefaultJson $ 123 :*: RNil)
    it "encodes via DefaultJsonFormat using makeRecordJsonWrapper" $ do
      toJsonWithFormat defaultJsonFormat (FooRecordDefaultJson $ 123 :*: RNil) `shouldBe` [aesonQQ| {foo: 123} |]

    it "decodes via FromJSON using makeRecordJsonWrapperExplicit" $ do
      Aeson.parseEither Aeson.parseJSON [aesonQQ| {foo: 123} |] `shouldBe` Right (FooRecordExplicitJson $ 113 :*: RNil)
    it "encodes via ToJSON using makeRecordJsonWrapperExplicit" $ do
      Aeson.toJSON (FooRecordExplicitJson $ 113 :*: RNil) `shouldBe` [aesonQQ| {foo: 123} |]
    it "decodes via DefaultJsonFormat using makeRecordJsonWrapperExplicit" $ do
      ABE.parseValue @Void (fromJsonWithFormat defaultJsonFormat) [aesonQQ| {foo: 123} |] `shouldBe` Right (FooRecordExplicitJson $ 113 :*: RNil)
    it "encodes via DefaultJsonFormat using makeRecordJsonWrapperExplicit" $ do
      toJsonWithFormat defaultJsonFormat (FooRecordExplicitJson $ 113 :*: RNil) `shouldBe` [aesonQQ| {foo: 123} |]

    it "decodes via FromJSON using makeFieldJsonWrapper" $ do
      Aeson.parseEither Aeson.parseJSON [aesonQQ| {foo: 123} |] `shouldBe` Right (FooFieldDefaultJson $ review _FFoo 123)
    it "encodes via ToJSON using makeFieldJsonWrapper" $ do
      Aeson.toJSON (FooFieldDefaultJson $ review _FFoo 123) `shouldBe` [aesonQQ| {foo: 123} |]
    it "decodes via DefaultJsonFormat using makeFieldJsonWrapper" $ do
      ABE.parseValue @Void (fromJsonWithFormat defaultJsonFormat) [aesonQQ| {foo: 123} |] `shouldBe` Right (FooFieldDefaultJson $ review _FFoo 123)
    it "encodes via DefaultJsonFormat using makeFieldJsonWrapper" $ do
      toJsonWithFormat defaultJsonFormat (FooFieldDefaultJson $ review _FFoo 123) `shouldBe` [aesonQQ| {foo: 123} |]

    it "decodes via FromJSON using makeFieldJsonWrapperExplicit" $ do
      Aeson.parseEither Aeson.parseJSON [aesonQQ| {foo: 123} |] `shouldBe` Right (FooFieldExplicitJson $ review _FFoo 113)
    it "encodes via ToJSON using makeFieldJsonWrapperExplicit" $ do
      Aeson.toJSON (FooFieldExplicitJson $ review _FFoo 113) `shouldBe` [aesonQQ| {foo: 123} |]
    it "decodes via DefaultJsonFormat using makeFieldJsonWrapperExplicit" $ do
      ABE.parseValue @Void (fromJsonWithFormat defaultJsonFormat) [aesonQQ| {foo: 123} |] `shouldBe` Right (FooFieldExplicitJson $ review _FFoo 113)
    it "encodes via DefaultJsonFormat using makeFieldJsonWrapperExplicit" $ do
      toJsonWithFormat defaultJsonFormat (FooFieldExplicitJson $ review _FFoo 113) `shouldBe` [aesonQQ| {foo: 123} |]
