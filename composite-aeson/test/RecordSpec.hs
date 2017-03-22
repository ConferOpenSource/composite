module RecordSpec where

import BasicPrelude
import Composite.Aeson.Base (JsonFormat, fromJsonWithFormat, toJsonWithFormat)
import Composite.Aeson.Formats.Provided (stringJsonFormat)
import Composite.Aeson.Record (defaultJsonFormatRec, recJsonFormat, optionalField)
import Composite.TH (withLensesAndProxies)
import Control.Lens (set)
import Data.Aeson.BetterErrors (parseValue)
import Data.Aeson.QQ (aesonQQ)
import Data.Vinyl.Lens (rlens)
import Data.Void (Void)
import Frames (Record, (:->), (&:), pattern Nil)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

withLensesAndProxies [d|
  type FFoo = "foo" :-> Int
  type FBar = "bar" :-> Maybe String
  |]
type TestRec = '["foo" :-> Int, "bar" :-> Maybe String]

recordSuite :: Spec
recordSuite =
  describe "Record support" $ do
    let defaultFmt, optionalFmt :: JsonFormat Void (Record TestRec)
        defaultFmt = recJsonFormat defaultJsonFormatRec
        optionalFmt = recJsonFormat $ set (rlens fBar_) (optionalField stringJsonFormat) defaultJsonFormatRec

    it "by default requires all fields" $ do
      parseValue (fromJsonWithFormat defaultFmt) [aesonQQ| {foo: 123, bar: "abc"} |] `shouldBe`    Right (123 &: Just "abc" &: Nil)
      parseValue (fromJsonWithFormat defaultFmt) [aesonQQ| {foo: 123, bar: null}  |] `shouldBe`    Right (123 &: Nothing    &: Nil)
      parseValue (fromJsonWithFormat defaultFmt) [aesonQQ| {foo: 123}             |] `shouldNotBe` Right (123 &: Nothing    &: Nil)

    it "by default encodes all fields" $ do
      toJsonWithFormat defaultFmt (123 &: Just "abc" &: Nil) `shouldBe` [aesonQQ| {foo: 123, bar: "abc"} |]
      toJsonWithFormat defaultFmt (123 &: Nothing    &: Nil) `shouldBe` [aesonQQ| {foo: 123, bar: null}  |]

    it "can make fields optional" $ do
      parseValue (fromJsonWithFormat optionalFmt) [aesonQQ| {foo: 123, bar: "abc"} |] `shouldBe` Right (123 &: Just "abc" &: Nil)
      parseValue (fromJsonWithFormat optionalFmt) [aesonQQ| {foo: 123, bar: null}  |] `shouldBe` Right (123 &: Nothing    &: Nil)
      parseValue (fromJsonWithFormat optionalFmt) [aesonQQ| {foo: 123}             |] `shouldBe` Right (123 &: Nothing    &: Nil)

    it "omits Nothing fields when they're made optional" $ do
      toJsonWithFormat optionalFmt (123 &: Just "abc" &: Nil) `shouldBe` [aesonQQ| {foo: 123, bar: "abc"} |]
      toJsonWithFormat optionalFmt (123 &: Nothing    &: Nil) `shouldBe` [aesonQQ| {foo: 123}             |]
