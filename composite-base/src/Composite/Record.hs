module Composite.Record
  ( Rec((:&), RNil), Record
  , pattern (:*:), pattern (:^:)
  , (:->)(Val, getVal), valName, valWithName
  , RElem, rlens, rlens'
  ) where

import BasicPrelude
import Control.Lens.TH (makeWrapped)
import Data.Functor.Identity (Identity(Identity))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup (Semigroup)
import Data.Text (pack)
import Data.Vinyl (Rec((:&), RNil))
import qualified Data.Vinyl as Vinyl
import qualified Data.Vinyl.TypeLevel as Vinyl
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
pattern (:^:) fa rs <- (map getVal -> fa) :& rs where
  (:^:) fa rs = map Val fa :& rs
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
  Vinyl.rlens proxy $ \ (map getVal -> fa) ->
    map Val <$> f fa
{-# INLINE rlens' #-}
