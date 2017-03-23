module RecordSpec where

import BasicPrelude
import Composite.Record
import Composite.TH (withLensesAndProxies)
import Test.Hspec (Spec, describe, it, shouldBe)

withLensesAndProxies [d|
  type FFoo = "foo" :-> Int
  type FBar = "bar" :-> Maybe String
  |]
type TestRec = '["foo" :-> Int, "bar" :-> Maybe String]

recordSuite :: Spec
recordSuite = do
  describe "Basic record utilities" $ do
    it "Supports construction and deconstruction of a Rec Identity" $ do
      let rec = 123 :*: Nothing :*: Nil :: Record TestRec
          foo :*: bar :*: Nil = rec
      foo `shouldBe` 123
      bar `shouldBe` Nothing

    it "Supports construction and deconstruction of a Rec f" $ do
      let rec = Just 123 :^: Nothing :^: Nil :: Rec Maybe TestRec
          Just foo :^: Nothing :^: Nil = rec
      foo `shouldBe` 123

    it "Supports pattern matching an Identity .: (:->)" $ do
      let val = Val (123 :: Int)
          Val i = val
      i `shouldBe` 123

