module Composite.Aeson.Record where

import BasicPrelude
import Composite.Aeson.Base
  ( ToJson(ToJson)
  , FromJson(FromJson)
  , JsonProfunctor(JsonProfunctor), _JsonProfunctor
  , JsonFormat(JsonFormat)
  , wrappedFormat
  )
import Composite.Base (NamedField(fieldName))
import Composite.Aeson.Default (DefaultJsonFormat(defaultJsonFormat))
import Control.Lens (Wrapped(type Unwrapped, _Wrapped'), _1, _2, view)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec((:&), RNil), rmap)
import Data.Vinyl.Functor (Identity(Identity))

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
--   userFormatRec = 'Composite.Aeson.Default.integralJsonFormat'
--                &: 'Composite.Aeson.Default.textJsonFormat'
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
type JsonFormatRec e rs = Rec (JsonFormat e) rs

-- |Helper class which induces over the structure of a record, reflecting the name of each field and applying each 'ToJson' to its corresponding value to
-- produce JSON.
class RecToJsonObject rs where
  -- |Given a record of 'ToJson' functions for each field in @rs@, convert an 'Identity' record to 'Aeson.Object'.
  recToJsonObject :: Rec ToJson rs -> Rec Identity rs -> Aeson.Object

instance RecToJsonObject '[] where
  recToJsonObject _ = const mempty

instance forall r rs. (NamedField r, RecToJsonObject rs) => RecToJsonObject (r ': rs) where
  recToJsonObject (ToJson aToJson :& fs) (Identity a :& as) =
    HM.insert (fieldName (Proxy :: Proxy r)) (aToJson a) $
      recToJsonObject fs as

-- |Given a record of 'ToJson' functions for each field in @rs@, convert an 'Identity' record to JSON. Equivalent to @Aeson.Object . 'recToJsonObject' fmt@
recToJson :: RecToJsonObject rs => Rec ToJson rs -> Rec Identity rs -> Aeson.Value
recToJson = map Aeson.Object . recToJsonObject

-- |Class which induces over the structure of a record, parsing fields using a record of 'FromJson' and assembling an 'Identity' record.
class RecFromJson rs where
  -- |Given a record of 'FromJson' parsers for each field in @rs@, produce an 'ABE.Parse' to make an 'Identity' record.
  recFromJson :: Rec (FromJson e) rs -> ABE.Parse e (Rec Identity rs)

instance RecFromJson '[] where
  recFromJson _ = pure RNil

instance forall r rs. (NamedField r, RecFromJson rs) => RecFromJson (r ': rs) where
  recFromJson (FromJson aFromJson :& fs) =
    (:&)
      <$> ABE.key (fieldName (Proxy :: Proxy r)) (Identity <$> aFromJson)
      <*> recFromJson fs

-- |Take a 'JsonFormatRec' describing how to map a record with field @rs@ to and from JSON and produce a @'JsonFormat' e (Record rs)@.
--
-- See 'JsonFormatRec' for more.
recJsonFormat :: (RecToJsonObject rs, RecFromJson rs) => JsonFormatRec e rs -> JsonFormat e (Rec Identity rs)
recJsonFormat formatRec =
  JsonFormat $ JsonProfunctor
    (recToJson   . rmap (view (_Wrapped' . _JsonProfunctor . _1)) $ formatRec)
    (recFromJson . rmap (view (_Wrapped' . _JsonProfunctor . _2)) $ formatRec)

-- |Class to make a 'JsonFormatRec' with 'defaultJsonFormat' for each field.
class DefaultJsonFormatRec rs where
  -- |Produce a 'JsonFormatRec' for a record with fields @rs@ by using the default 'JsonFormat' for each field in @rs@, as provided by 'DefaultJsonFormat'.
  defaultJsonFormatRec :: JsonFormatRec e rs

instance (NamedField r, DefaultJsonFormat (Unwrapped r), DefaultJsonFormatRec rs) => DefaultJsonFormatRec (r ': rs) where
  defaultJsonFormatRec = wrappedFormat defaultJsonFormat :& defaultJsonFormatRec

instance DefaultJsonFormatRec '[] where
  defaultJsonFormatRec = RNil
