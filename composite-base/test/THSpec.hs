module THSpec where

import Composite.CoRecord (Field, field)
import Composite.Record ((:->)(Val), Rec(RNil), Record, pattern (:*:), rlens)
import Composite.TH
import Control.Lens (preview, review, view)
import Test.Hspec (Spec, describe, it, shouldBe)

-- test that withLensesAndProxies works in the simple case
withLensesAndProxies [d|
  type FConcrete = "foo" :-> Int
  |]

-- test that withLensesAndProxies ignores non-conformant shapes (without :->) in the declaration
withLensesAndProxies [d|
  type ConcreteRec = '[FConcrete]
  type FConcreteRec = "foo" :-> Record ConcreteRec
  |]

-- test that withLensesAndProxies works with parameterized fields (and unrelated types)
withLensesAndProxies [d|
  type FParameterized a = "foo" :-> a
  type ParameterizedRec a = '[FParameterized a]
  type FParameterizedRec a = "foo" :-> Record (ParameterizedRec a)
  |]

-- test that withOpticsAndProxies works in the simple case
withOpticsAndProxies [d|
  type FOptical = "foo" :-> Int
  |]

thSuite :: Spec
thSuite = do
  describe "withLensesAndProxies" $ do
    it "works for simple fields" $ do
      let r :: Record '[FConcrete]
          r = 123 :*: RNil
      view        fConcrete   r `shouldBe` 123
      view (rlens fConcrete_) r `shouldBe` 123

    it "works for declaration blocks with non-fields in them" $ do
      let r :: Record ConcreteRec
          r = 123 :*: RNil
          r2 :: Record '[FConcreteRec]
          r2 = r :*: RNil
      view (      fConcreteRec  . fConcrete) r2 `shouldBe` 123
      view (rlens fConcreteRec_ . fConcrete) r2 `shouldBe` 123

    it "works with parameterized fields" $ do
      let ra  :: Record '[FParameterized Int]
          ra = 123 :*: RNil
          ra2 :: Record '[FParameterizedRec Int]
          ra2 = ra :*: RNil
          rb  :: Record '[FParameterized Bool]
          rb = True :*: RNil
          rb2 :: Record '[FParameterizedRec Bool]
          rb2 = rb :*: RNil

      view (                                  (fParameterized  @Int )) ra  `shouldBe` 123
      view (                            rlens (fParameterized_ @Int )) ra  `shouldBe` 123
      view (                                  (fParameterized  @Bool)) rb  `shouldBe` True
      view (                            rlens (fParameterized_ @Bool)) rb  `shouldBe` True
      view (      (fParameterizedRec  @Int ). (fParameterized  @Int )) ra2 `shouldBe` 123
      view (rlens (fParameterizedRec_ @Int ). (fParameterized  @Int )) ra2 `shouldBe` 123
      view (      (fParameterizedRec  @Bool). (fParameterized  @Bool)) rb2 `shouldBe` True
      view (rlens (fParameterizedRec_ @Bool). (fParameterized  @Bool)) rb2 `shouldBe` True

  describe "withOpticsAndProxies" $ do
    it "works for simple fields" $ do
      let f :: Field '[FOptical]
          f = field (Val 123 :: FOptical)
      preview _FOptical f `shouldBe` Just 123
      review _FOptical 123 `shouldBe` f


