{-# LANGUAGE DeriveGeneric #-}
module EnumSpec where

import Data.Void (Void)
import Data.Aeson (Value(String))
import GHC.Generics (Generic)
import Composite.Aeson.Base (JsonFormat, fromJsonWithFormat, toJsonWithFormat)
import Composite.Aeson.Enum (enumJsonFormat)
import Composite.Aeson.Formats.Provided (listJsonFormat)
import qualified Data.Aeson.BetterErrors as ABE
import Data.Aeson.QQ (aesonQQ)
import Test.Hspec (Spec, describe, it, shouldBe)

data TestEnum = A | B deriving (Show, Eq, Ord, Generic)

data LargerTestEnum = D | E | F | G deriving (Show, Eq, Ord, Generic)

testEnumFormat :: JsonFormat Void TestEnum
testEnumFormat = enumJsonFormat ""

largerTestEnumFormat :: JsonFormat Void LargerTestEnum
largerTestEnumFormat = enumJsonFormat ""

enumSuite :: Spec
enumSuite =
  describe "enumJsonFormat" $ do
    describe "when mapping ADTs with more than two branches" $ do
      it "should encode each value correctly" $
        toJsonWithFormat (listJsonFormat largerTestEnumFormat) [D,E,F,G]
          `shouldBe` [aesonQQ|["D", "E", "F", "G"]|]

      it "should decode each value correctly" $
        ABE.parseValue (fromJsonWithFormat (listJsonFormat largerTestEnumFormat)) [aesonQQ|["D", "E", "F", "G"]|]
          `shouldBe` Right [D,E,F,G]

    describe "when input value does not match any of enum constructors" $
      it "should return a parse error, not throw an exception" $
        ABE.parseValue (fromJsonWithFormat testEnumFormat) (String "C")
          `shouldBe` Left (ABE.BadSchema [] (ABE.FromAeson "expected one of A, B, not C"))
