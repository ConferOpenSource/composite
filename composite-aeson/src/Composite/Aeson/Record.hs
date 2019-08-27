module Composite.Aeson.Record
  ( ToField(..), FromField(..), JsonField(..), field, field', optionalField, optionalField'
  , JsonFormatRecord, DefaultJsonFormatRecord, defaultJsonFormatRecord
  , RecordToJsonObject, recordToJsonObject, recordToJson
  , RecordFromJson, recordFromJson
  , recordJsonFormat
  ) where

import Composite.Aeson.Base
  ( JsonProfunctor(JsonProfunctor)
  , JsonFormat(JsonFormat)
  , wrappedJsonFormat
  )
import Composite.Aeson.Formats.Default (DefaultJsonFormat(defaultJsonFormat))
import Composite.Record ((:->))
import Control.Lens (Wrapped(type Unwrapped, _Wrapped'), from, view)
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Functor.Identity (Identity(Identity))
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Vinyl (RMap, Rec((:&), RNil), rmap)
import GHC.TypeLits (KnownSymbol, symbolVal)

-- |Function to encode a single field of a record, possibly choosing to elide the field with @Nothing@.
newtype ToField a = ToField { unToField :: a -> Maybe Aeson.Value }

instance Contravariant ToField where
  contramap f (ToField g) = ToField (g . f)

-- |Function to decode a single field of a record.
newtype FromField e a = FromField { unFromField :: Text -> ABE.Parse e a }

instance Functor (FromField e) where
  fmap f (FromField g) = FromField (fmap f . g)

-- |Descriptor of how to handle a single record field with functions to parse and emit the field which can handle missing fields on parse and elide fields on
-- encode.
data JsonField e a = JsonField (a -> Maybe Aeson.Value) (Text -> ABE.Parse e a)

-- |Given a 'JsonFormat' for some type @a@, produce a 'JsonField' for fields of type @a@ which fails if the field is missing and never elides the field.
field :: (Wrapped a', Unwrapped a' ~ a) => JsonFormat e a -> JsonField e a'
field fmt = field' (wrappedJsonFormat fmt)

-- |Given a 'JsonFormat' for some type @a@, produce a 'JsonField' for fields of type @a@ which fails if the field is missing and never elides the field.
field' :: JsonFormat e a -> JsonField e a
field' (JsonFormat (JsonProfunctor o i)) = JsonField (Just . o) (`ABE.key` i)

-- |Given a 'JsonFormat' for some type @a@, produce a 'JsonField' for fields of type @Maybe a@ which substitutes @Nothing@ for either @null@ or missing field,
-- and which elides the field on @Nothing@.
optionalField :: (Wrapped a', Unwrapped a' ~ Maybe a) => JsonFormat e a -> JsonField e a'
optionalField (JsonFormat (JsonProfunctor o i)) =
  JsonField
    (fmap o . view _Wrapped')
    (\ k -> view (from _Wrapped') . join <$> ABE.keyMay k (ABE.perhaps i))

-- |Given a 'JsonFormat' for some type @a@, produce a 'JsonField' for fields of type @Maybe a@ which substitutes @Nothing@ for either @null@ or missing field,
-- and which elides the field on @Nothing@.
optionalField' :: JsonFormat e a -> JsonField e (Maybe a)
optionalField' (JsonFormat (JsonProfunctor o i)) =
  JsonField
    (fmap o)
    (\ k -> join <$> ABE.keyMay k (ABE.perhaps i))

-- |Type of a Vinyl/Frames record which describes how to map fields of a record to JSON and back.
--
-- This record type has the same field names and types as a regular record with 'Identity' but instead of 'Identity' uses 'JsonFormat e'.
--
-- For example, given:
--
-- > type FId   = "id"   :-> Int
-- > type FName = "name" :-> Text
-- > type User = '[FId, FName]
--
-- A 'JsonFormatRecord' for @User@ might be:
--
-- @
--   userFormatRec :: 'JsonFormatRecord' e User
--   userFormatRec = 'field' 'Composite.Aeson.Default.integralJsonFormat'
--                :& 'field' 'Composite.Aeson.Default.textJsonFormat'
--                :& RNil
-- @
--
-- Or, using the default mappings for each field type:
--
-- @
--   userFormatRec :: 'JsonFormatRecord' e User
--   userFormatRec = 'defaultJsonFormatRecord'
-- @
--
-- Such a record is a first-class value like any other record, so can be composed into larger records, modified, etc. This is particularly useful in
-- combination with 'defaultJsonFormatRecord', where you can automatically derive a format record for all fields you want defaults for and then extend or
-- override formats for particular fields, e.g.
--
-- @
--   fId :: Proxy FId
--   fId = Proxy
--
--   userFormatRec :: 'JsonFormatRecord' e User
--   userFormatRec = 'Control.Lens.over' ('Frames.rlens' fId) ('Composite.Aeson.Base.dimapJsonFormat (+10) (subtract 10)) 'defaultJsonFormatRecord'
-- @
--
-- Would use the same JSON schema as the other examples, but the @id@ field would be encoded in JSON as 10 higher.
--
-- Once you've produced an appropriate 'JsonFormatRecord' for your case, use 'recordJsonFormat' to make a @'JsonFormat' e (Record '[…])@ of it.
type JsonFormatRecord e rs = Rec (JsonField e) rs

-- |Helper class which induces over the structure of a record, reflecting the name of each field and applying each 'ToJson' to its corresponding value to
-- produce JSON.
class RecordToJsonObject rs where
  -- |Given a record of 'ToField' functions for each field in @rs@, convert an 'Identity' record to 'Aeson.Object'.
  recordToJsonObject :: Rec ToField rs -> Rec Identity rs -> Aeson.Object

instance RecordToJsonObject '[] where
  recordToJsonObject _ = const mempty

instance forall s a rs. (KnownSymbol s, RecordToJsonObject rs) => RecordToJsonObject (s :-> a ': rs) where
  recordToJsonObject (ToField aToField :& fs) (Identity a :& as) =
    maybe id (HM.insert (pack . symbolVal $ (Proxy :: Proxy s))) (aToField a) $
      recordToJsonObject fs as

-- |Given a record of 'ToField' functions for each field in @rs@, convert an 'Identity' record to JSON. Equivalent to @Aeson.Object . 'recordToJsonObject' fmt@
recordToJson :: RecordToJsonObject rs => Rec ToField rs -> Rec Identity rs -> Aeson.Value
recordToJson = fmap Aeson.Object . recordToJsonObject

-- |Class which induces over the structure of a record, parsing fields using a record of 'FromJson' and assembling an 'Identity' record.
class RecordFromJson rs where
  -- |Given a record of 'FromJson' parsers for each field in @rs@, produce an 'ABE.Parse' to make an 'Identity' record.
  recordFromJson :: Rec (FromField e) rs -> ABE.Parse e (Rec Identity rs)

instance RecordFromJson '[] where
  recordFromJson _ = pure RNil

instance forall s a rs. (KnownSymbol s, RecordFromJson rs) => RecordFromJson (s :-> a ': rs) where
  recordFromJson (FromField aFromField :& fs) =
    (:&)
      <$> (Identity <$> aFromField (pack . symbolVal $ (Proxy :: Proxy s)))
      <*> recordFromJson fs

-- |Take a 'JsonFormatRecord' describing how to map a record with field @rs@ to and from JSON and produce a @'JsonFormat' e (Record rs)@.
--
-- See 'JsonFormatRecord' for more.
recordJsonFormat :: (RMap rs, RecordToJsonObject rs, RecordFromJson rs) => JsonFormatRecord e rs -> JsonFormat e (Rec Identity rs)
recordJsonFormat formatRec =
  JsonFormat $ JsonProfunctor
    (recordToJson   . rmap (\ (JsonField o _) -> ToField o  ) $ formatRec)
    (recordFromJson . rmap (\ (JsonField _ i) -> FromField i) $ formatRec)

-- |Class to make a 'JsonFormatRecord' with 'defaultJsonFormat' for each field.
class DefaultJsonFormatRecord rs where
  -- |Produce a 'JsonFormatRecord' for a record with fields @rs@ by using the default 'JsonFormat' for each field in @rs@, as provided by 'DefaultJsonFormat'.
  defaultJsonFormatRecord :: JsonFormatRecord e rs

instance (KnownSymbol s, DefaultJsonFormat a, DefaultJsonFormatRecord rs) => DefaultJsonFormatRecord (s :-> a ': rs) where
  defaultJsonFormatRecord = field defaultJsonFormat :& defaultJsonFormatRecord

instance DefaultJsonFormatRecord '[] where
  defaultJsonFormatRecord = RNil
