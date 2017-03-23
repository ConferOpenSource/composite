module RecordSpec where

import BasicPrelude
import Composite.Record
import Composite.TH (withLensesAndProxies)
import Control.Lens (view, set, _Just)
import Test.Hspec (Spec, describe, it, shouldBe)

withLensesAndProxies [d|
  type FFoo = "foo" :-> Int
  type FBar = "bar" :-> String
  |]
type TestRec = '["foo" :-> Int, "bar" :-> String]

recordSuite :: Spec
recordSuite = do
  describe "Basic record utilities" $ do
    it "Supports construction and deconstruction of a Rec Identity" $ do
      let rec = 123 :*: "foo" :*: Nil :: Record TestRec
          foo :*: bar :*: Nil = rec
      foo `shouldBe` 123
      bar `shouldBe` "foo"

    it "Supports construction and deconstruction of a Rec f" $ do
      let rec = Just 123 :^: Nothing :^: Nil :: Rec Maybe TestRec
          Just foo :^: Nothing :^: Nil = rec
      foo `shouldBe` 123

    it "Supports pattern matching an Identity .: (:->)" $ do
      let val = Val (123 :: Int)
          Val i = val
      i `shouldBe` 123

    it "Supports lensing in a Rec Identity" $ do
      let rec = 123 :*: "foo" :*: Nil :: Record TestRec
      view (rlens fFoo_) rec `shouldBe` 123
      view (rlens fBar_) rec `shouldBe` "foo"
      set (rlens fFoo_) 321   rec `shouldBe` (321 :*: "foo" :*: Nil)
      set (rlens fBar_) "bar" rec `shouldBe` (123 :*: "bar" :*: Nil)

    it "Supports lensing in a Rec Maybe" $ do
      let rec = Just 123 :^: Nothing :^: Nil :: Rec Maybe TestRec
      view (rlens' fFoo_) rec `shouldBe` Just 123
      view (rlens' fBar_) rec `shouldBe` Nothing
      set (rlens' fFoo_ . _Just) 321   rec `shouldBe` (Just 321 :^: Nothing :^: Nil)
      set (rlens' fBar_ . _Just) "bar" rec `shouldBe` (Just 123 :^: Nothing :^: Nil)

