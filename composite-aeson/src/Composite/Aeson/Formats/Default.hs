module Composite.Aeson.Formats.Default
  ( DefaultJsonFormat(..)
  ) where

import BasicPrelude
import Composite.Aeson.Base (JsonFormat, wrappedJsonFormat)
import Composite.Aeson.Formats.Generic (aesonJsonFormat)
import Composite.Aeson.Formats.InternalTH (makeTupleDefaults)
import Composite.Aeson.Formats.Provided -- sorry
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as Aeson
import Data.Functor.Compose (Compose)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Int (Int8, Int16)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Monoid as Monoid
import Data.Scientific (Scientific)
import qualified Data.Semigroup as Semigroup
import Data.Tagged (Tagged)
import Data.Vector (Vector)
import Data.Version (Version)
import Data.Word (Word8, Word16)
import Numeric.Natural (Natural)

-- |Class for associating a default JSON format with a type.
--
-- DO NOT use this as the primary interface. It should only be used for defaulting in contexts where an explicit choice can also be used.
--
-- Instances of this class are (hopefully) provided for each type with an obviously correct interpretation, for example 'Text', 'Int', etc. Conversely types
-- without an obviously correct interpretation and in particular those with many contradictory interpretations are not included, for example 'UTCTime',
-- forcing you to choose one.
--
-- For types with surprising JSON mapping characteristics, take time and consider whether it would be better to explicitly configure what format to use
-- instead of providing a default.
class DefaultJsonFormat a where
  -- |Produce the default 'JsonFormat' for type @a@, which must not produce any custom errors.
  defaultJsonFormat :: JsonFormat e a

  -- |Produce the default 'JsonFormat' for a list of @a@, which must not produce any custom errors.
  -- This function does not usually need to be implemented as it has a sensible default. It exists to avoid overlapping instances, e.g. for @Char@
  -- and @String ~ [Char]@. The default implementation uses 'listJsonFormat'.
  defaultJsonFormatList :: JsonFormat e [a]
  defaultJsonFormatList = listJsonFormat defaultJsonFormat

instance DefaultJsonFormat a => DefaultJsonFormat (Identity a)                where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Semigroup.Min a)           where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Semigroup.Max a)           where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Semigroup.First a)         where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Semigroup.Last a)          where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Semigroup.WrappedMonoid a) where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Semigroup.Option a)        where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Monoid.Dual a)             where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Monoid.Sum a)              where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Monoid.Product a)          where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Monoid.First a)            where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Monoid.Last a)             where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Const a b)                 where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Tagged b a)                where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Maybe a)                   where defaultJsonFormat = maybeJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (NonEmpty a)                where defaultJsonFormat = nonEmptyListJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Seq a)                     where defaultJsonFormat = seqJsonFormat defaultJsonFormat
instance DefaultJsonFormat a => DefaultJsonFormat (Vector a)                  where defaultJsonFormat = vectorJsonFormat defaultJsonFormat

instance DefaultJsonFormat (f (g a)) => DefaultJsonFormat (Compose f g a) where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat

instance DefaultJsonFormat (f a) => DefaultJsonFormat (Monoid.Alt f a) where defaultJsonFormat = wrappedJsonFormat defaultJsonFormat

instance (FromJSONKey k, ToJSONKey k, Ord k, FromJSON a, ToJSON a) => DefaultJsonFormat (Map k a) where
  defaultJsonFormat = aesonJsonFormat -- because of the insanity that is FromJSONKey / ToJSONKey

instance (FromJSONKey k, ToJSONKey k, Eq k, Hashable k, FromJSON a, ToJSON a) => DefaultJsonFormat (HashMap k a) where
  defaultJsonFormat = aesonJsonFormat -- because of the insanity that is FromJSONKey / ToJSONKey

$makeTupleDefaults

instance DefaultJsonFormat Monoid.All where defaultJsonFormat = wrappedJsonFormat boolJsonFormat
instance DefaultJsonFormat Monoid.Any where defaultJsonFormat = wrappedJsonFormat boolJsonFormat

instance DefaultJsonFormat Aeson.Value  where defaultJsonFormat = aesonValueJsonFormat
instance DefaultJsonFormat Bool         where defaultJsonFormat = boolJsonFormat
instance DefaultJsonFormat IntSet       where defaultJsonFormat = intSetJsonFormat
instance DefaultJsonFormat Int          where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int8         where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int16        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int32        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int64        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Integer      where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word         where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word8        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word16       where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word32       where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word64       where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat LText        where defaultJsonFormat = lazyTextJsonFormat
instance DefaultJsonFormat Natural      where defaultJsonFormat = naturalJsonFormat
instance DefaultJsonFormat Ordering     where defaultJsonFormat = orderingJsonFormat
instance DefaultJsonFormat Float        where defaultJsonFormat = realFloatJsonFormat
instance DefaultJsonFormat Double       where defaultJsonFormat = realFloatJsonFormat
instance DefaultJsonFormat Scientific   where defaultJsonFormat = scientificJsonFormat
instance DefaultJsonFormat Text         where defaultJsonFormat = textJsonFormat
instance DefaultJsonFormat ()           where defaultJsonFormat = unitJsonFormat
instance DefaultJsonFormat Version      where defaultJsonFormat = versionJsonFormat

instance DefaultJsonFormat Char where
  defaultJsonFormat = charJsonFormat
  defaultJsonFormatList = stringJsonFormat

instance DefaultJsonFormat a => DefaultJsonFormat [a] where
  defaultJsonFormat = defaultJsonFormatList
