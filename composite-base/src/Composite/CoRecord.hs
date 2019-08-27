-- |Module containing the sum formulation companion to 'Composite.Record's product formulation. Values of type @'CoRec' f rs@ represent a single value
-- @f r@ for one of the @r@s in @rs@. Heavily based on the great work by Anthony Cowley in Frames.
{-# LANGUAGE UndecidableInstances #-} -- for FoldRec
module Composite.CoRecord where

import Prelude
import Composite.Record (AllHave, HasInstances, (:->)(getVal, Val), reifyDicts, reifyVal, val, zipRecsWith)
import Control.Lens (Prism', prism')
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl.Core (Dict(Dict), Rec((:&), RNil), RMap, RecApplicative, RecordToList, ReifyConstraint, recordToList, reifyConstraint, rmap, rpure)
import Data.Vinyl.Functor (Compose(Compose, getCompose), Const(Const), (:.))
import Data.Vinyl.Lens (RElem, type (∈), type (⊆), rget, rput, rreplace)
import Data.Vinyl.TypeLevel (RecAll, RIndex)

-- FIXME? replace with int-index/union or at least lift ideas from there. This encoding is awkward to work with and not compositional.

-- |@CoRef f rs@ represents a single value of type @f r@ for some @r@ in @rs@.
data CoRec :: (u -> *) -> [u] -> * where
  -- |Witness that @r@ is an element of @rs@ using '∈' ('RElem' with 'RIndex') from Vinyl.
  CoVal :: r ∈ rs => !(f r) -> CoRec f rs

instance forall rs. (AllHave '[Show] rs, RecApplicative rs) => Show (CoRec Identity rs) where
  show (CoVal (Identity x)) = "(CoVal " ++ show' x ++ ")"
    where
      shower :: Rec (Op String) rs
      shower = reifyDicts (Proxy @'[Show]) (\ _ -> Op show)
      show' = runOp (rget shower)

instance forall rs. (RMap rs, RecAll Maybe rs Eq, RecApplicative rs, RecordToList rs, ReifyConstraint Eq Maybe rs) => Eq (CoRec Identity rs) where
  crA == crB = and . recordToList $ zipRecsWith f (toRec crA) (fieldToRec crB)
    where
      f :: forall a. (Dict Eq :. Maybe) a -> Maybe a -> Const Bool a
      f (Compose (Dict a)) b = Const $ a == b
      toRec = reifyConstraint . fieldToRec

-- |The common case of a 'CoRec' with @f ~ 'Identity'@, i.e. a regular value.
type Field = CoRec Identity

-- |Inject a value @f r@ into a @'CoRec' f rs@ given that @r@ is one of the valid @rs@.
--
-- Equivalent to 'CoVal' the constructor, but exists to parallel 'field'.
coRec :: r ∈ rs => f r -> CoRec f rs
coRec = CoVal

-- |Produce a prism for the given alternative of a 'CoRec'.
coRecPrism :: (RecApplicative rs, r ∈ rs) => Prism' (CoRec f rs) (f r)
coRecPrism = prism' CoVal (getCompose . rget . coRecToRec)

-- |Inject a value @r@ into a @'Field' rs@ given that @r@ is one of the valid @rs@.
--
-- Equivalent to @'CoVal' . 'Identity'@.
field :: r ∈ rs => r -> Field rs
field = CoVal . Identity

-- |Inject a value @a@ into a @'Field' rs@ given that @s :-> a@ is one of the valid @rs@.
--
-- Equivalent to @'CoVal' . 'Identity' . 'Val'@.
fieldVal :: forall s a rs proxy. s :-> a ∈ rs => proxy (s :-> a) -> a -> Field rs
fieldVal _ = CoVal . val @s

-- |Produce a prism for the given alternative of a 'Field'.
fieldPrism :: (RecApplicative rs, r ∈ rs) => Prism' (Field rs) r
fieldPrism = coRecPrism . dimap runIdentity (fmap Identity)

-- |Produce a prism for the given @:->@ alternative of a 'Field', given a proxy to identify which @s :-> a@ you meant.
fieldValPrism :: (RecApplicative rs, s :-> a ∈ rs) => proxy (s :-> a) -> Prism' (Field rs) a
fieldValPrism proxy = coRecPrism . dimap (getVal . reifyVal proxy . runIdentity) (fmap (Identity . Val))

-- |Apply an extraction to whatever @f r@ is contained in the given 'CoRec'.
--
-- For example @foldCoVal getConst :: CoRec (Const a) rs -> a@.
foldCoVal :: (forall (r :: u). RElem r rs (RIndex r rs) => f r -> b) -> CoRec f rs -> b
foldCoVal f (CoVal x) = f x
{-# INLINE foldCoVal #-}

-- |Map a @'CoRec' f@ to a @'CoRec' g@ using a natural transform from @f@ to @g@ (@forall x. f x -> g x@).
mapCoRec :: (forall x. f x -> g x) -> CoRec f rs -> CoRec g rs
mapCoRec f (CoVal x) = CoVal (f x)
{-# INLINE mapCoRec #-}

-- |Apply some kleisli on @h@ to the @f x@ contained in a @'CoRec' f@ and yank the @h@ outside. Like 'traverse' but for 'CoRec'.
traverseCoRec :: Functor h => (forall x. f x -> h (g x)) -> CoRec f rs -> h (CoRec g rs)
traverseCoRec f (CoVal x) = CoVal <$> f x
{-# INLINE traverseCoRec #-}

-- |Project a @'CoRec' f@ into a @'Rec' ('Maybe' ':.' f)@ where only the single @r@ held by the 'CoRec' is 'Just' in the resulting record, and all other
-- fields are 'Nothing'.
coRecToRec :: RecApplicative rs => CoRec f rs -> Rec (Maybe :. f) rs
coRecToRec (CoVal a) = rput (Compose (Just a)) (rpure (Compose Nothing))
{-# INLINE coRecToRec #-}

-- |Project a 'Field' into a @'Rec' 'Maybe'@ where only the single @r@ held by the 'Field' is 'Just' in the resulting record, and all other
-- fields are 'Nothing'.
fieldToRec :: (RMap rs, RecApplicative rs) => Field rs -> Rec Maybe rs
fieldToRec = rmap (fmap runIdentity . getCompose) . coRecToRec
{-# INLINE fieldToRec #-}

-- |Typeclass which allows folding ala 'foldMap' over a 'Rec', using a 'CoRec' as the accumulator.
class FoldRec ss ts where
  -- |Given some combining function, an initial value, and a record, visit each field of the record using the combining function to accumulate the
  -- initial value or previous accumulation with the field of the record.
  foldRec
    :: (CoRec f ss -> CoRec f ss -> CoRec f ss)
    -> CoRec f ss
    -> Rec f ts
    -> CoRec f ss

instance FoldRec ss '[] where
  foldRec _ z _ = z
  {-# INLINE foldRec #-}

instance (t ∈ ss, FoldRec ss ts) => FoldRec ss (t ': ts) where
  foldRec f z (x :& xs) = foldRec f (z `f` CoVal x) xs
  {-# INLINE foldRec #-}

-- |'foldRec' for records with at least one field that doesn't require an initial value.
foldRec1
  :: FoldRec (r ': rs) rs
  => (CoRec f (r ': rs) -> CoRec f (r ': rs) -> CoRec f (r ': rs))
  -> Rec f (r ': rs)
  -> CoRec f (r ': rs)
foldRec1 f (x :& xs) = foldRec f (CoVal x) xs
{-# INLINE foldRec1 #-}

-- |Given a @'Rec' ('Maybe' ':.' f) rs@, yield a @Just coRec@ for the first field which is @Just@, or @Nothing@ if there are no @Just@ fields in the record.
firstCoRec :: FoldRec rs rs => Rec (Maybe :. f) rs -> Maybe (CoRec f rs)
firstCoRec RNil       = Nothing
firstCoRec v@(x :& _) = traverseCoRec getCompose $ foldRec f (CoVal x) v
  where
    f c@(CoVal (Compose (Just _))) _ = c
    f _                            c = c
{-# INLINE firstCoRec #-}

-- |Given a @'Rec' 'Maybe' rs@, yield a @Just field@ for the first field which is @Just@, or @Nothing@ if there are no @Just@ fields in the record.
firstField :: (FoldRec rs rs, RMap rs) => Rec Maybe rs -> Maybe (Field rs)
firstField = firstCoRec . rmap (Compose . fmap Identity)
{-# INLINE firstField #-}

-- |Given a @'Rec' ('Maybe' ':.' f) rs@, yield a @Just coRec@ for the last field which is @Just@, or @Nothing@ if there are no @Just@ fields in the record.
lastCoRec :: FoldRec rs rs => Rec (Maybe :. f) rs -> Maybe (CoRec f rs)
lastCoRec RNil       = Nothing
lastCoRec v@(x :& _) = traverseCoRec getCompose $ foldRec f (CoVal x) v
  where
    f _ c@(CoVal (Compose (Just _))) = c
    f c                            _ = c
{-# INLINE lastCoRec #-}

-- |Given a @'Rec' 'Maybe' rs@, yield a @Just field@ for the last field which is @Just@, or @Nothing@ if there are no @Just@ fields in the record.
lastField :: (RMap rs, FoldRec rs rs) => Rec Maybe rs -> Maybe (Field rs)
lastField = lastCoRec . rmap (Compose . fmap Identity)
{-# INLINE lastField #-}

-- |Helper newtype containing a function @a -> b@ but with the type parameters flipped so @Op b@ has a consistent codomain for a varying domain.
newtype Op b a = Op { runOp :: a -> b }

-- |Given a list of constraints @cs@ required to apply some function, apply the function to whatever value @r@ (not @f r@) which the 'CoRec' contains.
onCoRec
  :: forall (cs :: [* -> Constraint]) (f :: * -> *) (rs :: [*]) (b :: *) (proxy :: [* -> Constraint] -> *).
     (AllHave cs rs, Functor f, RecApplicative rs)
  => proxy cs
  -> (forall (a :: *). HasInstances a cs => a -> b)
  -> CoRec f rs
  -> f b
onCoRec p f (CoVal x) = go <$> x
  where
    go = runOp $ rget (reifyDicts p (\ _ -> Op f) :: Rec (Op b) rs)
{-# INLINE onCoRec #-}

-- |Given a list of constraints @cs@ required to apply some function, apply the function to whatever value @r@ which the 'Field' contains.
onField
  :: forall (cs :: [* -> Constraint]) (rs :: [*]) (b :: *) (proxy :: [* -> Constraint] -> *).
     (AllHave cs rs, RecApplicative rs)
  => proxy cs
  -> (forall (a :: *). HasInstances a cs => a -> b)
  -> Field rs
  -> b
onField p f x = runIdentity (onCoRec p f x)
{-# INLINE onField #-}

-- |Given some target type @r@ that's a possible value of @'Field' rs@, yield @Just@ if that is indeed the value being stored by the 'Field', or @Nothing@ if
-- not.
asA :: (r ∈ rs, RMap rs, RecApplicative rs) => Field rs -> Maybe r
asA = rget . fieldToRec
{-# INLINE asA #-}

-- |An extractor function @f a -> b@ which can be passed to 'foldCoRec' to eliminate one possible alternative of a 'CoRec'.
newtype Case' f b a = Case' { unCase' :: f a -> b }

-- |A record of @Case'@ eliminators for each @r@ in @rs@ representing the pieces of a total function from @'CoRec' f@ to @b@.
type Cases' f rs b = Rec (Case' f b) rs

-- |Fold a @'CoRec' f@ using @Cases'@ which eliminate each possible value held by the 'CoRec', yielding the @b@ produced by whichever case matches.
foldCoRec :: RecApplicative (r ': rs) => Cases' f (r ': rs) b -> CoRec f (r ': rs) -> b
foldCoRec hs = go hs . coRecToRec
  where
    go :: Cases' f rs b -> Rec (Maybe :. f) rs -> b
    go (Case' f :&  _) (Compose (Just x) :& _) = f x
    go (Case' _ :& fs) (Compose Nothing  :& t) = go fs t
    go RNil            RNil                    = error "foldCoRec should be provably total, isn't"
    {-# INLINE go #-}
{-# INLINE foldCoRec #-}

-- |Fold a @'CoRec' f@ using @Cases'@ which eliminate each possible value held by the 'CoRec', yielding the @b@ produced by whichever case matches.
--
-- Equivalent to 'foldCoRec' but with its arguments flipped so it can be written @matchCoRec coRec $ cases@.
matchCoRec :: RecApplicative (r ': rs) => CoRec f (r ': rs) -> Cases' f (r ': rs) b -> b
matchCoRec = flip foldCoRec
{-# INLINE matchCoRec #-}

newtype Case b a = Case { unCase :: a -> b }
type Cases rs b = Rec (Case b) rs

-- |Fold a 'Field' using 'Cases' which eliminate each possible value held by the 'Field', yielding the @b@ produced by whichever case matches.
foldField :: (RMap rs, RecApplicative (r ': rs)) => Cases (r ': rs) b -> Field (r ': rs) -> b
foldField hs = foldCoRec (rmap (Case' . (. runIdentity) . unCase) hs)
{-# INLINE foldField #-}

-- |Fold a 'Field' using 'Cases' which eliminate each possible value held by the 'Field', yielding the @b@ produced by whichever case matches.
--
-- Equivalent to 'foldCoRec' but with its arguments flipped so it can be written @matchCoRec coRec $ cases@.
matchField :: (RMap rs, RecApplicative (r ': rs)) => Field (r ': rs) -> Cases (r ': rs) b -> b
matchField = flip foldField
{-# INLINE matchField #-}

-- |Widen a @'CoRec' f rs@ to a @'CoRec' f ss@ given that @rs ⊆ ss@.
widenCoRec :: (FoldRec ss ss, RecApplicative rs, RecApplicative ss, rs ⊆ ss) => CoRec f rs -> CoRec f ss
widenCoRec r =
  fromMaybe (error "widenCoRec should be provably total, isn't") $
    firstCoRec (rreplace (coRecToRec r) (rpure $ Compose Nothing))

-- |Widen a @'Field' rs@ to a @'Field' ss@ given that @rs ⊆ ss@.
widenField :: (FoldRec ss ss, RMap rs, RMap ss, RecApplicative rs, RecApplicative ss, rs ⊆ ss) => Field rs -> Field ss
widenField r =
  fromMaybe (error "widenField should be provably total, isn't") $
    firstField (rreplace (fieldToRec r) (rpure Nothing))
