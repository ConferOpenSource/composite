{-# LANGUAGE UndecidableInstances #-} -- argh, for ReifyNames
module Composite.Record
  ( Rec((:&), RNil), Record
  , pattern (:*:), pattern (:^:)
  , (:->)(Val, getVal), valName, valWithName
  , RElem, rlens, rlens'
  , AllHave, HasInstances, ValuesAllHave
  , zipRecsWith, reifyDicts, recordToNonEmpty
  , ReifyNames(reifyNames)
  , RecWithContext(rmapWithContext)
  ) where

import Control.Lens.TH (makeWrapped)
import Data.Functor.Identity (Identity(Identity))
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup (Semigroup)
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Vinyl (Rec((:&), RNil), RecApplicative, recordToList, rpure)
import qualified Data.Vinyl as Vinyl
import Data.Vinyl.Functor (Compose(Compose), Const(Const), (:.))
import Data.Vinyl.Lens (type (∈))
import qualified Data.Vinyl.TypeLevel as Vinyl
import Foreign.Storable (Storable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

type Record = Rec Identity
type RElem r rs = Vinyl.RElem r rs (Vinyl.RIndex r rs)

newtype (:->) (s :: Symbol) a = Val { getVal :: a }

makeWrapped ''(:->)

deriving instance Bounded    a => Bounded    (s :-> a)
deriving instance Enum       a => Enum       (s :-> a)
deriving instance Eq         a => Eq         (s :-> a)
deriving instance Floating   a => Floating   (s :-> a)
deriving instance Fractional a => Fractional (s :-> a)
deriving instance Integral   a => Integral   (s :-> a)
deriving instance IsString   a => IsString   (s :-> a)
deriving instance Monoid     a => Monoid     (s :-> a)
deriving instance Num        a => Num        (s :-> a)
deriving instance Ord        a => Ord        (s :-> a)
deriving instance Real       a => Real       (s :-> a)
deriving instance RealFloat  a => RealFloat  (s :-> a)
deriving instance RealFrac   a => RealFrac   (s :-> a)
deriving instance Semigroup  a => Semigroup  (s :-> a)
deriving instance Storable   a => Storable   (s :-> a)

instance Functor ((:->) s) where
  fmap f = Val . f . getVal
  {-# INLINE fmap #-}
instance Applicative ((:->) s) where
  pure = Val
  {-# INLINE pure #-}
  Val f <*> Val a = Val (f a)
  {-# INLINE (<*>) #-}
instance Foldable ((:->) s) where
  foldr f z (Val a) = f a z
  {-# INLINE foldr #-}
instance Traversable ((:->) s) where
  traverse k (Val a) = Val <$> k a
  {-# INLINE traverse #-}
instance Monad ((:->) s) where
  return = Val
  {-# INLINE return #-}
  Val a >>= k = k a
  {-# INLINE (>>=) #-}

instance forall (s :: Symbol) a. (KnownSymbol s, Show a) => Show (s :-> a) where
  showsPrec p (Val a) = ((symbolVal (Proxy :: Proxy s) ++ " :-> ") ++) . showsPrec p a

-- |Reflect the type level name of a named value @s :-> a@ to a @Text@. For example, given @"foo" :-> Int@, yields @"foo" :: Text@
valName :: forall s a. KnownSymbol s => s :-> a -> Text
valName _ = pack (symbolVal (Proxy :: Proxy s))
{-# INLINE valName #-}

-- |Extract the value and reflect the name of a named value.
valWithName :: forall s a. KnownSymbol s => s :-> a -> (Text, a)
valWithName v = (valName v, getVal v)
{-# INLINE valWithName #-}

-- |Bidirectional pattern matching the first field of a record using ':->' values and the 'Identity' functor.
--
-- This pattern is bidirectional meaning you can use it either as a pattern or a constructor, e.g.
--
-- @
--   let rec = 123 :*: Just "foo" :*: RNil
--       foo :*: bar :*: RNil = rec
-- @
--
-- Mnemonic: @*@ for products.
pattern (:*:) :: () => () => a -> Rec Identity rs -> Rec Identity (s :-> a ': rs)
pattern (:*:) a rs = Identity (Val a) :& rs
infixr 5 :*:

-- |Bidirectional pattern matching the first field of a record using ':->' values and any functor.
--
-- This pattern is bidirectional meaning you can use it either as a pattern or a constructor, e.g.
--
-- @
--   let rec = Just 123 :^: Just "foo" :^: RNil
--       Just foo :^: Just bar :^: RNil = rec
-- @
--
-- Mnemonic: @^@ for products (record) of products (functor).
pattern (:^:) :: Functor f => () => f a -> Rec f rs -> Rec f (s :-> a ': rs)
pattern (:^:) fa rs <- (fmap getVal -> fa) :& rs where
  (:^:) fa rs = fmap Val fa :& rs
infixr 5 :^:

-- |Lens to a particular field of a record using the 'Identity' functor.
--
-- For example, given:
--
-- @
--   type FFoo = "foo" :-> Int
--   type FBar = "bar" :-> String
--   fBar_ :: Proxy FBar
--   fBar_ = Proxy
--
--   rec :: 'Rec' 'Identity' '[FFoo, FBar]
--   rec = 123 :*: "hello!" :*: Nil
-- @
--
-- Then:
--
-- @
--   view (rlens fBar_)               rec == "hello!"
--   set  (rlens fBar_) "goodbye!"    rec == 123 :*: "goodbye!" :*: Nil
--   over (rlens fBar_) (map toUpper) rec == 123 :*: "HELLO!"   :*: Nil
-- @
rlens :: (Functor g, RElem (s :-> a) rs, Functor g) => proxy (s :-> a) -> (a -> g a) -> Rec Identity rs -> g (Rec Identity rs)
rlens proxy f =
  Vinyl.rlens proxy $ \ (Identity (Val a)) ->
    Identity . Val <$> f a
{-# INLINE rlens #-}

-- |Lens to a particular field of a record using any functor.
--
-- For example, given:
--
-- @
--   type FFoo = "foo" :-> Int
--   type FBar = "bar" :-> String
--   fBar_ :: Proxy FBar
--   fBar_ = Proxy
--
--   rec :: 'Rec' 'Maybe' '[FFoo, FBar]
--   rec = Just 123 :^: Just "hello!" :^: Nil
-- @
--
-- Then:
--
-- @
--   view (rlens' fBar_)                      rec == Just "hello!"
--   set  (rlens' fBar_) Nothing              rec == Just 123 :^: Nothing       :^: Nil
--   over (rlens' fBar_) (fmap (map toUpper)) rec == Just 123 :^: Just "HELLO!" :^: Nil
-- @
rlens' :: (Functor f, Functor g, RElem (s :-> a) rs, Functor g) => proxy (s :-> a) -> (f a -> g (f a)) -> Rec f rs -> g (Rec f rs)
rlens' proxy f =
  Vinyl.rlens proxy $ \ (fmap getVal -> fa) ->
    fmap Val <$> f fa
{-# INLINE rlens' #-}

-- | 'zipWith' for Rec's.
zipRecsWith :: (forall a. f a -> g a -> h a) -> Rec f as -> Rec g as -> Rec h as
zipRecsWith _ RNil      _         = RNil
zipRecsWith f (r :& rs) (s :& ss) = f r s :& zipRecsWith f rs ss

-- | Convert a provably nonempty @'Rec' ('Const' a) rs@ to a @'NonEmpty' a@.
recordToNonEmpty :: Rec (Const a) (r ': rs) -> NonEmpty a
recordToNonEmpty (Const a :& rs) = a :| recordToList rs

-- |Type function which produces a constraint on @a@ for each constraint in @cs@.
--
-- For example, @HasInstances Int '[Eq, Ord]@ is equivalent to @(Eq Int, Ord Int)@.
type family HasInstances (a :: u) (cs :: [u -> Constraint]) :: Constraint where
  HasInstances a '[] = ()
  HasInstances a (c ': cs) = (c a, HasInstances a cs)

-- |Type function which produces the cross product of constraints @cs@ and types @as@.
--
-- For example, @AllHave '[Eq, Ord] '[Int, Text]@ is equivalent to @(Eq Int, Ord Int, Eq Text, Ord Text)@
type family AllHave (cs :: [u -> Constraint]) (as :: [u]) :: Constraint where
  AllHave cs      '[]  = ()
  AllHave cs (a ': as) = (HasInstances a cs, AllHave cs as)

-- |Type function which produces the cross product of constraints @cs@ and the values carried in a record @rs@.
--
-- For example, @ValuesAllHave '[Eq, Ord] '["foo" :-> Int, "bar" :-> Text]@ is equivalent to @(Eq Int, Ord Int, Eq Text, Ord Text)@
type family ValuesAllHave (cs :: [u -> Constraint]) (as :: [u]) :: Constraint where
  ValuesAllHave cs            '[]  = ()
  ValuesAllHave cs (s :-> a ': as) = (HasInstances a cs, ValuesAllHave cs as)


-- |Given a list of constraints @cs@, apply some function for each @r@ in the target record type @rs@ with proof that those constraints hold for @r@,
-- generating a record with the result of each application.
reifyDicts
  :: forall (cs :: [u -> Constraint]) (f :: u -> *) (rs :: [u]) (proxy :: [u -> Constraint] -> *).
     (AllHave cs rs, RecApplicative rs)
  => proxy cs
  -> (forall proxy' (a :: u). HasInstances a cs => proxy' a -> f a)
  -> Rec f rs
reifyDicts _ f = go (rpure (Const ()))
  where
    go :: forall (rs' :: [u]). AllHave cs rs' => Rec (Const ()) rs' -> Rec f rs'
    go RNil = RNil
    go ((_ :: Const () a) :& xs) = f (Proxy @a) :& go xs
{-# INLINE reifyDicts #-}

-- |Class which reifies the symbols of a record composed of ':->' fields as 'Text'.
class ReifyNames (rs :: [*]) where
  -- |Given a @'Rec' f rs@ where each @r@ in @rs@ is of the form @s ':->' a@, make a record which adds the 'Text' for each @s@.
  reifyNames :: Rec f rs -> Rec ((,) Text :. f) rs

instance ReifyNames '[] where
  reifyNames _ = RNil

instance forall (s :: Symbol) a (rs :: [*]). (KnownSymbol s, ReifyNames rs) => ReifyNames (s :-> a ': rs) where
  reifyNames (fa :& rs) = Compose ((,) (pack $ symbolVal (Proxy @s)) fa) :& reifyNames rs

-- |Class with 'Data.Vinyl.rmap' but which gives the natural transformation evidence that the value its working over is contained within the overall record @ss@.
class RecWithContext (ss :: [*]) (ts :: [*]) where
  -- |Apply a natural transformation from @f@ to @g@ to each field of the given record, except that the natural transformation can be mildly unnatural by having
  -- evidence that @r@ is in @ss@.
  rmapWithContext :: proxy ss -> (forall r. r ∈ ss => f r -> g r) -> Rec f ts -> Rec g ts

instance RecWithContext ss '[] where
  rmapWithContext _ _ _ = RNil

instance forall r (ss :: [*]) (ts :: [*]). (r ∈ ss, RecWithContext ss ts) => RecWithContext ss (r ': ts) where
  rmapWithContext proxy n (r :& rs) = n r :& rmapWithContext proxy n rs

