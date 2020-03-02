module Composite.Aeson.CoRecord
  ( ToJsonFormatField, FromJsonFormatField, JsonFormatField
  , DefaultJsonFormatField(defaultJsonFormatField)
  , fieldToJson, fieldFromJson, fieldJsonFormat
  ) where

import Composite.Aeson.Base (FromJson(FromJson), JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor), ToJson(ToJson), wrappedJsonFormat)
import Composite.Aeson.Formats.Default (DefaultJsonFormat, defaultJsonFormat)
import Composite.Aeson.Formats.Generic (SumStyle, jsonSumFormat, sumToJson, sumFromJson)
import Composite.CoRecord (CoRec(CoVal), Field, fieldToRec)
import Composite.Record ((:->), Rec((:&), RNil), RecWithContext(rmapWithContext), recordToNonEmpty, ReifyNames, reifyNames)
import Data.Aeson (Value)
import qualified Data.Aeson.BetterErrors as ABE
import Data.Functor.Identity (Identity(Identity))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Vinyl (RApply, RMap, RecApplicative, RecordToList, rapply, recordToList, (<<&>>))
import Data.Vinyl.Functor (Compose(Compose), (:.), Const(Const), Lift(Lift))
import Data.Vinyl.Lens (type (∈))
import Data.Proxy (Proxy(Proxy))

-- |Type of records which contain JSON encoders for each element of @rs@.
type ToJsonFormatField rs = Rec ToJson rs

-- |Type of records which contain JSON decoders for each element of @rs@.
type FromJsonFormatField e rs = Rec (FromJson e) rs

-- |Type of records which contain JSON formats for each element of @rs@.
type JsonFormatField e rs = Rec (JsonFormat e) rs

-- |Class which makes up a 'JsonFormatField' for some @rs@ where each @r ~ s :-> a@ by using the 'DefaultJsonFormat' instance for each @a@.
class DefaultJsonFormatField (rs :: [*]) where
  -- |Make up a 'JsonFormatField' for some @rs@ where each @r ~ s :-> a@ by using the 'DefaultJsonFormat' instance for each @a@.
  defaultJsonFormatField :: JsonFormatField e rs

instance DefaultJsonFormatField '[] where
  defaultJsonFormatField = RNil

instance forall s a rs. (DefaultJsonFormat a, DefaultJsonFormatField rs) => DefaultJsonFormatField (s :-> a ': rs) where
  defaultJsonFormatField = wrappedJsonFormat defaultJsonFormat :& (defaultJsonFormatField :: JsonFormatField e rs)

-- |Make a @'Field' rs -> 'Value'@ given how to map the sum type to JSON along with a record with encoders for each value the field could have.
fieldToJson
  :: forall (rs :: [*]) r' (rs' :: [*]).
     ( rs ~ (r' ': rs'), RApply rs, RMap rs
     , RecApplicative rs, RecWithContext rs rs, RecordToList rs', ReifyNames rs )
  => SumStyle -> ToJsonFormatField rs -> Field rs -> Value
fieldToJson sumStyle fmts = sumToJson sumStyle o
  where
    namedFmts :: Rec ((,) Text :. ToJson) rs
    namedFmts = reifyNames fmts

    o :: Field rs -> (Text, Value)
    o = fromMaybe (error "fieldToRec somehow produced all Nothings")
      . listToMaybe . catMaybes
      . (recordToList :: Rec (Const (Maybe (Text, Value))) rs -> [Maybe (Text, Value)])
      . rapply outputs
      . fieldToRec

    outputs :: Rec (Lift (->) Maybe (Const (Maybe (Text, Value)))) rs
    outputs = namedFmts <<&>> \ (Compose (name, ToJson oa)) ->
      Lift $ Const . fmap ((name,) . oa)

-- |Make a @'ABE.Parse' e (Field rs)@ given how to map the sum type from JSON along with a record with decoders for each value the field could have.
fieldFromJson
  :: forall (rs :: [*]) r' (rs' :: [*]) e.
     ( rs ~ (r' ': rs'), RApply rs, RMap rs
     , RecApplicative rs, RecWithContext rs rs, RecordToList rs', ReifyNames rs )
  => SumStyle -> FromJsonFormatField e rs -> ABE.Parse e (Field rs)
fieldFromJson sumStyle fmts = sumFromJson sumStyle i
  where
    namedFmts :: Rec ((,) Text :. FromJson e) rs
    namedFmts = reifyNames fmts

    i :: NonEmpty (Text, FromJson e (Field rs))
    i = recordToNonEmpty $ rmapWithContext (Proxy @rs) oneCase namedFmts
      where
        oneCase :: forall r. r ∈ rs => ((,) Text :. FromJson e) r -> Const (Text, FromJson e (Field rs)) r
        oneCase (Compose (name, FromJson ia)) =
          Const (name, FromJson (CoVal . Identity <$> ia))

-- |Make a @'JsonFormat' e (Field rs)@ given how to map the sum type to JSON along with a record with formatters for each value the field could have.
fieldJsonFormat
  :: forall (rs :: [*]) r' (rs' :: [*]) e.
     ( rs ~ (r' ': rs'), RApply rs, RMap rs
     , RecApplicative rs, RecWithContext rs rs, RecordToList rs', ReifyNames rs )
  => SumStyle -> JsonFormatField e rs -> JsonFormat e (Field rs)
fieldJsonFormat sumStyle fmts = jsonSumFormat sumStyle o i
  where
    namedFmts :: Rec ((,) Text :. JsonFormat e) rs
    namedFmts = reifyNames fmts

    o :: Field rs -> (Text, Value)
    o = fromMaybe (error "fieldToRec somehow produced all Nothings")
      . listToMaybe . catMaybes
      . (recordToList :: Rec (Const (Maybe (Text, Value))) rs -> [Maybe (Text, Value)])
      . rapply outputs
      . fieldToRec

    outputs :: Rec (Lift (->) Maybe (Const (Maybe (Text, Value)))) rs
    outputs = namedFmts <<&>> \ (Compose (name, JsonFormat (JsonProfunctor oa _))) ->
      Lift $ Const . fmap ((name,) . oa)

    i :: NonEmpty (Text, FromJson e (Field rs))
    i = recordToNonEmpty $ rmapWithContext (Proxy @rs) oneCase namedFmts
      where
        oneCase :: forall r. r ∈ rs => ((,) Text :. JsonFormat e) r -> Const (Text, FromJson e (Field rs)) r
        oneCase (Compose (name, JsonFormat (JsonProfunctor _ ia))) =
          Const (name, FromJson (CoVal . Identity <$> ia))

