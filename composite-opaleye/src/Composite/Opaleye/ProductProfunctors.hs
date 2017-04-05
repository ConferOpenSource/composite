{-# OPTIONS_GHC -fno-warn-orphans #-}
module Composite.Opaleye.ProductProfunctors where

import BasicPrelude
import Composite.Record ((:->)(Val), Rec((:&), RNil))
import Data.Functor.Identity (Identity(Identity))
import Data.Profunctor (dimap)
import Data.Profunctor.Product (ProductProfunctor, (***!))
import qualified Data.Profunctor.Product as PP
import Data.Profunctor.Product.Default (Default(def))

-- |Type class implementing traversal of a record, yanking individual product profunctors @Record [p a b]@
-- (though with distinct @a@ and @b@ at each position) up to @p (Record as) (Record bs)@.
--
-- This is similar to the @pN@ functions on tuples provided by the @product-profunctors@ library.
class ProductProfunctor p => PRec p rs where
  -- |Record fields @rs@ with the profunctor removed yielding the contravariant parameter. E.g. @PRecContra p '[p a b] ~ '[a]@
  type PRecContra p rs :: [*]
  -- |Record fields @rs@ with the profunctor removed yielding the covariant parameter. E.g. @PRecContra p '[p a b] ~ '[a]@
  type PRecCo     p rs :: [*]

  -- |Traverse the record, transposing the profunctors @p@ within to the outside like 'traverse' does for Applicative effects.
  --
  -- Roughly equivalent to @Record '[p a b, p c d, …] -> p (Record '[a, c, …]) (Record '[b, d, …])@
  pRec :: Rec Identity rs -> p (Rec Identity (PRecContra p rs)) (Rec Identity (PRecCo p rs))

instance ProductProfunctor p => PRec p '[] where
  type PRecContra p '[] = '[]
  type PRecCo     p '[] = '[]

  pRec RNil = dimap (const ()) (const RNil) PP.empty

instance (ProductProfunctor p, PRec p rs) => PRec p (s :-> p a b ': rs) where
  type PRecContra p (s :-> p a b ': rs) = (s :-> a ': PRecContra p rs)
  type PRecCo     p (s :-> p a b ': rs) = (s :-> b ': PRecCo     p rs)

  pRec (Identity (Val p) :& rs) =
    dimap (\ (Identity (Val a) :& aRs) -> (a, aRs))
          (\ (b, bRs) -> (Identity (Val b) :& bRs))
          (p ***! pRec rs)

instance ProductProfunctor p => Default p (Rec Identity '[]) (Rec Identity '[]) where
  def = dimap (const ()) (const RNil) PP.empty

instance forall p s a b rsContra rsCo. (ProductProfunctor p, Default p a b, Default p (Rec Identity rsContra) (Rec Identity rsCo))
      => Default p (Rec Identity (s :-> a ': rsContra)) (Rec Identity (s :-> b ': rsCo)) where
  def =
    dimap (\ (Identity (Val a) :& aRs) -> (a, aRs))
          (\ (b, bRs) -> (Identity (Val b) :& bRs))
          (step ***! recur)
    where
      step :: p a b
      step = def
      recur :: p (Rec Identity rsContra) (Rec Identity rsCo)
      recur = def



