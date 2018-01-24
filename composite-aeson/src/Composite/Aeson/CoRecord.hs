module Composite.Aeson.CoRecord
  ( JsonFormatField, DefaultJsonFormatField(defaultJsonFormatField)
  , fieldJsonFormat
  ) where

import Composite.Aeson.Base (FromJson(FromJson), JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor), wrappedJsonFormat)
import Composite.Aeson.Formats.Default (DefaultJsonFormat, defaultJsonFormat)
import Composite.Aeson.Formats.Generic (SumStyle, jsonSumFormat)
import Composite.CoRecord (CoRec(CoVal), Field, fieldToRec)
import Composite.Record ((:->), Rec((:&), RNil), RecWithContext(rmapWithContext), recordToNonEmpty, ReifyNames, reifyNames)
import Data.Aeson (Value)
import Data.Functor.Identity (Identity(Identity))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Vinyl (RecApplicative, rapply, recordToList, (<<&>>))
import Data.Vinyl.Functor (Compose(Compose), (:.), Const(Const), Lift(Lift))
import Data.Vinyl.Lens (type (∈))
import Data.Proxy (Proxy(Proxy))

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

-- |Make a @'JsonFormat' e (Field rs)@ given how to map the sum type to JSON along with a record with formatters for each value the field could have.
fieldJsonFormat
  :: forall (rs :: [*]) r' (rs' :: [*]) e.
     (rs ~ (r' ': rs'), RecApplicative rs, RecWithContext rs rs, ReifyNames rs)
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

