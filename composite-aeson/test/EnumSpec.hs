{-# LANGUAGE DeriveGeneric #-}
module EnumSpec where

import Data.Void (Void)
import Data.Aeson (Value(String))
import GHC.Generics (Generic)
import Composite.Aeson.Base (JsonFormat, fromJsonWithFormat)
import Composite.Aeson.Enum (enumJsonFormat)
import qualified Data.Aeson.BetterErrors as ABE
import Test.Hspec (Spec, describe, it, shouldBe)

data TestEnum = A | B deriving (Show, Eq, Ord, Generic)

testEnumFormat :: JsonFormat Void TestEnum
testEnumFormat = enumJsonFormat ""

enumSuite :: Spec
enumSuite =
  describe "enumJsonFormat" $
    describe "when input value does not match any of enum constructors" $
      it "should return a parse error, not throw an exception" $
        ABE.parseValue (fromJsonWithFormat testEnumFormat) (String "C")
          `shouldBe` Left (ABE.BadSchema [] (ABE.FromAeson "expected one of A, B, not C"))
