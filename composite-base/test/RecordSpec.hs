module RecordSpec where

import Composite.Record
import Composite.TH (withLensesAndProxies)
import Control.Lens (set, view, _Just)
import Data.Functor.Contravariant (Predicate (Predicate, getPredicate))
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
      let rec = 123 :*: "foo" :*: RNil :: Record TestRec
          foo :*: bar :*: RNil = rec
      foo `shouldBe` 123
      bar `shouldBe` "foo"

    it "Supports construction and deconstruction of a Rec f" $ do
      let rec = Just 123 :^: Nothing :^: RNil :: Rec Maybe TestRec
          Just foo :^: Nothing :^: RNil = rec
      foo `shouldBe` 123

    it "Supports construction and deconstruction of a Rec f (Contravariant)" $ do
      let rec = Predicate even :!: Predicate (even . length) :!: RNil :: Rec Predicate TestRec
          foo :!: bar :!: RNil = rec
      getPredicate foo 123 `shouldBe` False
      getPredicate bar "foo" `shouldBe` False

    it "Supports lensing in a Rec Identity" $ do
      let rec = 123 :*: "foo" :*: RNil :: Record TestRec
      view (rlens fFoo_) rec `shouldBe` 123
      view (rlens fBar_) rec `shouldBe` "foo"
      set (rlens fFoo_) 321   rec `shouldBe` (321 :*: "foo" :*: RNil)
      set (rlens fBar_) "bar" rec `shouldBe` (123 :*: "bar" :*: RNil)

    it "Supports lensing in a Rec Maybe" $ do
      let rec = Just 123 :^: Nothing :^: RNil :: Rec Maybe TestRec
      view (rlensCo fFoo_) rec `shouldBe` Just 123
      view (rlensCo fBar_) rec `shouldBe` Nothing
      set (rlensCo fFoo_ . _Just) 321   rec `shouldBe` (Just 321 :^: Nothing :^: RNil)
      set (rlensCo fBar_ . _Just) "bar" rec `shouldBe` (Just 123 :^: Nothing :^: RNil)

    it "Supports lensing in a Rec Predicate" $ do
      let rec = Predicate even :!: Predicate (even . length) :!: RNil :: Rec Predicate TestRec
      getPredicate (view (rlensContra fFoo_) rec) 123 `shouldBe` False
      getPredicate (view (rlensContra fBar_) rec) "foo" `shouldBe` False
      getPredicate (view (rlensContra fFoo_) (set (rlensContra fFoo_) (Predicate odd) rec)) 123 `shouldBe` True
      getPredicate (view (rlensContra fBar_) (set (rlensContra fBar_) (Predicate (odd . length)) rec)) "foo" `shouldBe` True
