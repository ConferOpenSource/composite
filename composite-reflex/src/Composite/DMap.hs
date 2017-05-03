-- |Contains functionality to interoperate with DMap and DSum from dependent-map and dependent-sum.
module Composite.DMap
  ( Witness(WitnessHead, WitnessTail)
  , RecDSums(recToDSums, sortedDSumsToRec), recToDMap, dmapToRec
  ) where

import Prelude
import Composite.Record (Rec((:&), RNil))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum((:=>)))
import Data.GADT.Compare (GCompare, gcompare, GEq, geq, GOrdering(GEQ, GLT, GGT))
import Data.Type.Equality ((:~:)(Refl))
import Data.Vinyl.Functor (Compose(Compose), (:.))

-- |Witness that a particular position in a list of types is a particular type, used as a DMap/DSum key witness
data Witness (rs :: [u]) (r :: u) where
  -- |The target type @r@ is at the head of the type list.
  WitnessHead :: Witness (r ': rs) r
  -- |The target type @r@ is further down the type list, as proven by the argument @Witness@
  WitnessTail :: Witness rs r -> Witness (h ': rs) r

-- |Allow for generalized equality testing for 'Witness' keys
instance GEq (Witness rs) where
  WitnessHead   `geq` WitnessHead   = Just Refl
  WitnessHead   `geq` _             = Nothing
  _             `geq` WitnessHead   = Nothing
  WitnessTail a `geq` WitnessTail b = a `geq` b
  {-# INLINE geq #-}

-- |Allow for comparison of 'Witness' keys, which are ordered by their position in the original type list (record fields)
instance GCompare (Witness rs) where
  WitnessHead   `gcompare` WitnessHead   = GEQ
  WitnessHead   `gcompare` _             = GLT
  _             `gcompare` WitnessHead   = GGT
  WitnessTail a `gcompare` WitnessTail b = a `gcompare` b
  {-# INLINE gcompare #-}

-- |Class which allows @'Rec' f rs@s to be broken down into lists of @'DSum' ('Witness' rs) f@ and for (sorted) lists of @'DSum' ('Witness' rs) f@ to be
-- reconstituted into a @'Rec' ('Maybe' ':.' f) rs@.
class RecDSums (rs :: [u]) where
  -- |Decompose a @'Rec' f rs@ into a list of @'DSum' ('Witness' rs) f@, one for each field of the record.
  recToDSums :: forall (f :: u -> *). Rec f rs -> [DSum (Witness rs) f]

  -- |Assemble a list of @'DSum' ('Witness' rs) f@ which have been sorted according to the 'Witness' ordering back into a @'Rec' ('Maybe' :. f) rs@.
  --
  -- The resulting record is composed with 'Maybe' because there's no guarantee that the list of DSums covers all fields.
  --
  -- If more than one DSum given has the same Witness, the resulting field will take the first value.
  sortedDSumsToRec :: forall (f :: u -> *). [DSum (Witness rs) f] -> Rec (Maybe :. f) rs

instance RecDSums '[] where
  recToDSums _ = []
  {-# INLINE recToDSums #-}

  sortedDSumsToRec _ = RNil
  {-# INLINE sortedDSumsToRec #-}

instance forall (r :: u) (rs :: [u]). RecDSums rs => RecDSums (r ': rs) where
  recToDSums (r :& rs) = (WitnessHead :=> r) : fmap (\ (t :=> v) -> WitnessTail t :=> v) (recToDSums rs)
  {-# INLINE recToDSums #-}

  sortedDSumsToRec sums =
    case splitSums sums of
      (fa : _, ts) -> Compose (Just fa) :& sortedDSumsToRec ts
      ([]    , ts) -> Compose Nothing   :& sortedDSumsToRec ts
    where
      splitSums :: [DSum (Witness (r ': rs)) f] -> ([f r], [DSum (Witness rs) f])
      splitSums ((WitnessHead   :=> a) : rest) = let (as, ts) = splitSums rest in (a : as,             ts)
      splitSums ((WitnessTail t :=> a) : rest) = let ( _, ts) = splitSums rest in (    [], (t :=> a) : ts)
      splitSums []                              = ([], [])
  {-# INLINE sortedDSumsToRec #-}

recToDMap :: RecDSums rs => Rec f rs -> DMap (Witness rs) f
recToDMap = DMap.fromAscList . recToDSums
{-# INLINE recToDMap #-}

-- |Convert a @'DMap' ('Witness' rs) f@ into a into a @'Rec' ('Maybe' :. f) rs@.
--
-- The resulting record is composed with 'Maybe' because there's no guarantee that the fields of the DMap covers all fields required by the record.
--
-- If more than one DSum given has the same Witness, the resulting field will take the first value.
dmapToRec :: RecDSums rs => DMap (Witness rs) f -> Rec (Maybe :. f) rs
dmapToRec = sortedDSumsToRec . DMap.toAscList
{-# INLINE dmapToRec #-}
