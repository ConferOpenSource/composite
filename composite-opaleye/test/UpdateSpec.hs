module UpdateSpec where

import Composite.Opaleye.Update (recordToUpdate)
import Composite.Record ((:->), Rec(RNil), Record, pattern (:*:))
import Composite.TH (withLensesAndProxies)
import Test.Hspec (Spec, describe, it, shouldBe)

withLensesAndProxies [d|
  type FFoo    = "foo" :-> Int
  type FFooMay = "foo" :-> Maybe Int
  type FBar    = "bar" :-> String
  type FBaz    = "baz" :-> Maybe Double
  type FBazMay = "baz" :-> Maybe (Maybe Double)
  type FQux    = "qux" :-> Maybe Bool
  |]

type ForSelect1 = '[]
type ForUpdate1 = '[]

type ForSelect2 = '[FBar]
type ForUpdate2 = '[FBar]

type ForSelect3 = '[FFoo]
type ForUpdate3 = '[FFooMay]

type ForSelect4 = '[FFoo   , FBar, FBaz   , FQux]
type ForUpdate4 = '[FFooMay, FBar, FBazMay, FQux]

type ForSelect5 = '[FBar, FFoo   , FQux, FBaz   ]
type ForUpdate5 = '[FBar, FFooMay, FQux, FBazMay]

updateSuite :: Spec
updateSuite = do
  describe "Composite.Opaleye.Update" $ do
    it "should compute the update version of an empty record" $ do
      recordToUpdate (                                                     RNil :: Record ForSelect1)
        `shouldBe`   (                                                     RNil :: Record ForUpdate1)
    it "should compute the update version of a record with no defaulted fields" $ do
      recordToUpdate ("hi"                                             :*: RNil :: Record ForSelect2)
        `shouldBe`   ("hi"                                             :*: RNil :: Record ForUpdate2)
    it "should compute the update version of a record with a defaulted field" $ do
      recordToUpdate (     123                                         :*: RNil :: Record ForSelect3)
        `shouldBe`   (Just 123                                         :*: RNil :: Record ForUpdate3)
    it "should compute the update version of a record with mixed fields" $ do
      recordToUpdate (     123 :*: "hi" :*:      Nothing :*: Just True :*: RNil :: Record ForSelect4)
        `shouldBe`   (Just 123 :*: "hi" :*: Just Nothing :*: Just True :*: RNil :: Record ForUpdate4)
    it "should compute the update version of a record with mixed fields and different ordering" $ do
      recordToUpdate ("hi" :*:      123 :*: Just True :*:      Nothing :*: RNil :: Record ForSelect5)
        `shouldBe`   ("hi" :*: Just 123 :*: Just True :*: Just Nothing :*: RNil :: Record ForUpdate5)

