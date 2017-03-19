module Composite.Aeson.Record where

import BasicPrelude
import Composite.Aeson.Base
  ( JsonProfunctor(JsonProfunctor)
  , JsonFormat(JsonFormat)
  , wrappedJsonFormat
  )
import Composite.Base (NamedField(fieldName))
import Composite.Aeson.Formats.Default (DefaultJsonFormat(defaultJsonFormat))
import Control.Lens (Wrapped(type Unwrapped, _Wrapped'), from, view)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import Data.Functor.Contravariant (Contravariant, contramap)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec((:&), RNil), rmap)
import Data.Vinyl.Functor (Identity(Identity))

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
optionalField :: forall e a a'. (Wrapped a', Unwrapped a' ~ Maybe a) => JsonFormat e a -> JsonField e a'
optionalField (JsonFormat (JsonProfunctor o i)) =
  JsonField
    (map o . view _Wrapped')
    (\ k -> view (from _Wrapped') . join <$> ABE.keyMay k (ABE.perhaps i))

-- |Given a 'JsonFormat' for some type @a@, produce a 'JsonField' for fields of type @Maybe a@ which substitutes @Nothing@ for either @null@ or missing field,
-- and which elides the field on @Nothing@.
optionalField' :: JsonFormat e a -> JsonField e (Maybe a)
optionalField' (JsonFormat (JsonProfunctor o i)) =
  JsonField
    (map o)
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
-- A 'JsonFormatRec' for @User@ might be:
--
-- @
--   userFormatRec :: 'JsonFormatRec' e User
--   userFormatRec = 'field' 'Composite.Aeson.Default.integralJsonFormat'
--                &: 'field' 'Composite.Aeson.Default.textJsonFormat'
--                &: Nil
-- @
--
-- Or, using the default mappings for each field type:
--
-- @
--   userFormatRec :: 'JsonFormatRec' e User
--   userFormatRec = 'defaultJsonFormatRec'
-- @
--
-- Such a record is a first-class value like any other record, so can be composed into larger records, modified, etc. This is particularly useful in
-- combination with 'defaultJsonFormatRec', where you can automatically derive a format record for all fields you want defaults for and then extend or
-- override formats for particular fields, e.g.
--
-- @
--   fId :: Proxy FId
--   fId = Proxy
--
--   userFormatRec :: 'JsonFormatRec' e User
--   userFormatRec = 'Control.Lens.over' ('Frames.rlens' fId) ('Composite.Aeson.Base.dimapJsonFormat (+10) (subtract 10)) 'defaultJsonFormatRec'
-- @
--
-- Would use the same JSON schema as the other examples, but the @id@ field would be encoded in JSON as 10 higher.
--
-- Once you've produced an appropriate 'JsonFormatRec' for your case, use 'recJsonFormat' to make a @'JsonFormat' e (Record '[…])@ of it.
type JsonFormatRec e rs = Rec (JsonField e) rs

-- |Helper class which induces over the structure of a record, reflecting the name of each field and applying each 'ToJson' to its corresponding value to
-- produce JSON.
class RecToJsonObject rs where
  -- |Given a record of 'ToField' functions for each field in @rs@, convert an 'Identity' record to 'Aeson.Object'.
  recToJsonObject :: Rec ToField rs -> Rec Identity rs -> Aeson.Object

instance RecToJsonObject '[] where
  recToJsonObject _ = const mempty

instance forall r rs. (NamedField r, RecToJsonObject rs) => RecToJsonObject (r ': rs) where
  recToJsonObject (ToField aToField :& fs) (Identity a :& as) =
    maybe id (HM.insert (fieldName (Proxy :: Proxy r))) (aToField a) $
      recToJsonObject fs as

-- |Given a record of 'ToField' functions for each field in @rs@, convert an 'Identity' record to JSON. Equivalent to @Aeson.Object . 'recToJsonObject' fmt@
recToJson :: RecToJsonObject rs => Rec ToField rs -> Rec Identity rs -> Aeson.Value
recToJson = map Aeson.Object . recToJsonObject

-- |Class which induces over the structure of a record, parsing fields using a record of 'FromJson' and assembling an 'Identity' record.
class RecFromJson rs where
  -- |Given a record of 'FromJson' parsers for each field in @rs@, produce an 'ABE.Parse' to make an 'Identity' record.
  recFromJson :: Rec (FromField e) rs -> ABE.Parse e (Rec Identity rs)

instance RecFromJson '[] where
  recFromJson _ = pure RNil

instance forall r rs. (NamedField r, RecFromJson rs) => RecFromJson (r ': rs) where
  recFromJson (FromField aFromField :& fs) =
    (:&)
      <$> (Identity <$> aFromField (fieldName (Proxy :: Proxy r)))
      <*> recFromJson fs

-- |Take a 'JsonFormatRec' describing how to map a record with field @rs@ to and from JSON and produce a @'JsonFormat' e (Record rs)@.
--
-- See 'JsonFormatRec' for more.
recJsonFormat :: (RecToJsonObject rs, RecFromJson rs) => JsonFormatRec e rs -> JsonFormat e (Rec Identity rs)
recJsonFormat formatRec =
  JsonFormat $ JsonProfunctor
    (recToJson   . rmap (\ (JsonField o _) -> ToField o  ) $ formatRec)
    (recFromJson . rmap (\ (JsonField _ i) -> FromField i) $ formatRec)

-- |Class to make a 'JsonFormatRec' with 'defaultJsonFormat' for each field.
class DefaultJsonFormatRec rs where
  -- |Produce a 'JsonFormatRec' for a record with fields @rs@ by using the default 'JsonFormat' for each field in @rs@, as provided by 'DefaultJsonFormat'.
  defaultJsonFormatRec :: JsonFormatRec e rs

instance (NamedField r, DefaultJsonFormat (Unwrapped r), DefaultJsonFormatRec rs) => DefaultJsonFormatRec (r ': rs) where
  defaultJsonFormatRec = field defaultJsonFormat :& defaultJsonFormatRec

instance DefaultJsonFormatRec '[] where
  defaultJsonFormatRec = RNil
