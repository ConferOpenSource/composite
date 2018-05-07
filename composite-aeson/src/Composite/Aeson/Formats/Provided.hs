-- |Module which provides 'JsonFormat's for a variety of types from @base@ and other common packages.
module Composite.Aeson.Formats.Provided where

import Composite.Aeson.Base (JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor), _JsonProfunctor, dimapJsonFormat, toJsonWithFormat)
import Composite.Aeson.Formats.Generic (SumStyle, abeJsonFormat, aesonJsonFormat, jsonArrayFormat, jsonObjectFormat, jsonSumFormat)
import Composite.Aeson.Formats.InternalTH (makeTupleFormats, makeNamedTupleFormats)
import Control.Arrow (first)
import Control.Lens (_2, _Wrapped, over, view)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import Data.Fixed (HasResolution, Fixed)
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as LazyHashMap
import qualified Data.HashMap.Strict as StrictHashMap
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as StrictMap
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Sequence (Seq)
import qualified Data.Sequence as Sequence
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import Data.Version (Version)
import Numeric.Natural (Natural)


-- |JSON format for 'Aeson.Array' which maps to any array in JSON.
aesonArrayJsonFormat :: JsonFormat e Aeson.Array
aesonArrayJsonFormat = abeJsonFormat ABE.asArray

-- |JSON format for 'Aeson.Object' which maps to any object in JSON.
aesonObjectJsonFormat :: JsonFormat e Aeson.Object
aesonObjectJsonFormat = abeJsonFormat ABE.asObject

-- |JSON format which does no parsing or encoding.
aesonValueJsonFormat :: JsonFormat e Aeson.Value
aesonValueJsonFormat = abeJsonFormat ABE.asValue

-- |'JsonFormat' for 'Bool', mapping to a JSON boolean.
boolJsonFormat :: JsonFormat e Bool
boolJsonFormat = abeJsonFormat ABE.asBool

-- |'JsonFormat' for 'Char', mapping to a JSON string.
charJsonFormat :: JsonFormat e Char
charJsonFormat = aesonJsonFormat

-- |'JsonFormat' for 'Either' which maps to JSON as an object via 'jsonSumFormat'.
eitherJsonFormat :: SumStyle -> Text -> Text -> JsonFormat e a -> JsonFormat e b -> JsonFormat e (Either a b)
eitherJsonFormat style leftName rightName leftFormat rightFormat = jsonSumFormat style o is
  where
    o = \ case
      Left  a -> (leftName,  toJsonWithFormat leftFormat  a)
      Right b -> (rightName, toJsonWithFormat rightFormat b)
    is =
      (leftName, Left <$> view (_Wrapped . _JsonProfunctor . _2) leftFormat) :| [(rightName, Right <$> view (_Wrapped . _JsonProfunctor . _2) rightFormat)]

-- |'JsonFormat' for 'Fixed' precision real numbers.
fixedJsonFormat :: HasResolution r => JsonFormat e (Fixed r)
fixedJsonFormat = aesonJsonFormat

-- |'JsonFormat' for 'StrictHashMap.HashMap' where the key type can be converted to and from a 'Text', mapping to a JSON object.
strictHashMapJsonFormat :: (Eq k, Hashable k) => (k -> Text) -> (Text -> ABE.Parse e k) -> JsonFormat e a -> JsonFormat e (StrictHashMap.HashMap k a)
strictHashMapJsonFormat kToText kFromText =
  jsonObjectFormat (fmap (first kToText) . StrictHashMap.toList)
                   (fmap StrictHashMap.fromList . traverse (\ (k, a) -> (, a) <$> kFromText k))

-- |'JsonFormat' for 'LazyHashMap.HashMap' where the key type can be converted to and from a 'Text', mapping to a JSON object.
lazyHashMapJsonFormat :: (Eq k, Hashable k) => (k -> Text) -> (Text -> ABE.Parse e k) -> JsonFormat e a -> JsonFormat e (LazyHashMap.HashMap k a)
lazyHashMapJsonFormat kToText kFromText =
  jsonObjectFormat (fmap (first kToText) . LazyHashMap.toList)
                   (fmap LazyHashMap.fromList . traverse (\ (k, a) -> (, a) <$> kFromText k))

-- |'JsonFormat' for 'IntSet' which maps to an array of numbers.
intSetJsonFormat :: JsonFormat e IntSet
intSetJsonFormat = aesonJsonFormat

-- |Polymorphic JSON format for any type which instances 'Integral'.
integralJsonFormat :: Integral a => JsonFormat e a
integralJsonFormat = JsonFormat $ JsonProfunctor (Aeson.Number . fromIntegral) ABE.asIntegral

-- |'JsonFormat' for 'Data.Text.Lazy.Text'.
lazyTextJsonFormat :: JsonFormat e LT.Text
lazyTextJsonFormat = dimapJsonFormat LT.toStrict LT.fromStrict textJsonFormat

-- |'JsonFormat' for '[]' which maps to a JSON array.
listJsonFormat :: JsonFormat e a -> JsonFormat e [a]
listJsonFormat = jsonArrayFormat id pure

-- |'JsonFormat' for 'StrictMap.Map' where the key type can be converted to and from a 'Text', mapping to a JSON object.
strictMapJsonFormat :: Ord k => (k -> Text) -> (Text -> ABE.Parse e k) -> JsonFormat e a -> JsonFormat e (StrictMap.Map k a)
strictMapJsonFormat kToText kFromText =
  jsonObjectFormat (fmap (first kToText) . StrictMap.toAscList)
                   (fmap StrictMap.fromList . traverse (\ (k, a) -> (, a) <$> kFromText k))

-- |'JsonFormat' for 'LazyMap.Map' where the key type can be converted to and from a 'Text', mapping to a JSON object.
lazyMapJsonFormat :: Ord k => (k -> Text) -> (Text -> ABE.Parse e k) -> JsonFormat e a -> JsonFormat e (LazyMap.Map k a)
lazyMapJsonFormat kToText kFromText =
  jsonObjectFormat (fmap (first kToText) . LazyMap.toAscList)
                   (fmap LazyMap.fromList . traverse (\ (k, a) -> (, a) <$> kFromText k))

-- |'JsonFormat' for 'Maybe' which maps @Nothing@ to @null@.
maybeJsonFormat :: JsonFormat e a -> JsonFormat e (Maybe a)
maybeJsonFormat =
  over _Wrapped $ \ (JsonProfunctor o i) ->
    JsonProfunctor (maybe Aeson.Null o) (ABE.perhaps i)

-- |'JsonFormat' for 'Natural' numbers.
naturalJsonFormat :: JsonFormat e Natural
naturalJsonFormat = aesonJsonFormat

-- |'JsonFormat' for 'NonEmpty' which maps to a JSON array.
nonEmptyListJsonFormat :: JsonFormat e a -> JsonFormat e (NonEmpty a)
nonEmptyListJsonFormat =
  jsonArrayFormat NEL.toList (maybe (fail "expected nonempty array") pure . NEL.nonEmpty)

-- |JSON format for '()' which maps to JSON as @null@.
nullJsonFormat :: JsonFormat e ()
nullJsonFormat = abeJsonFormat ABE.asNull

-- |JSON format for 'Ordering' which maps to the strings @LT@, @GT@, and @EQ@
orderingJsonFormat :: JsonFormat e Ordering
orderingJsonFormat = aesonJsonFormat

-- |Polymorphic JSON format for any type which instances 'RealFloat'. See warning in documentation for 'scientificJsonFormat' about scientific notation.
realFloatJsonFormat :: RealFloat a => JsonFormat e a
realFloatJsonFormat = JsonFormat $ JsonProfunctor realFloatToJson ABE.asRealFloat

-- |Convert some 'RealFloat' value to 'Aeson.Value'. Copied from Aeson internals which do not export it.
realFloatToJson :: RealFloat a => a -> Aeson.Value
realFloatToJson d
  | isNaN d || isInfinite d = Aeson.Null
  | otherwise = Aeson.Number $ Scientific.fromFloatDigits d
{-# INLINE realFloatToJson #-}

-- |'JsonFormat' for 'Scientific', mapping to a JSON number.
--
-- __Warning:__ some JSON parsing libraries do not accept the scientific number notation even though it's part of the JSON standard, and this format
-- uses 'Data.ByteString.Builder.Scientific.scientificBuilder' transitively which encodes very small (\< 0.1) and large (> 9,999,999.0) fractional numbers
-- using scientific notation.
scientificJsonFormat :: JsonFormat e Scientific
scientificJsonFormat = abeJsonFormat ABE.asScientific

-- |'JsonFormat' for 'Seq'.
seqJsonFormat :: JsonFormat e a -> JsonFormat e (Seq a)
seqJsonFormat = jsonArrayFormat toList (pure . Sequence.fromList)

-- |'JsonFormat' for 'String'.
stringJsonFormat :: JsonFormat e String
stringJsonFormat = abeJsonFormat ABE.asString

-- |'JsonFormat' for arbitrary sum types which maps to JSON as an object with fields determined by the 'SumStyle' chosen. See 'SumStyle' for more information
-- about the various styles.

-- |'JsonFormat' for 'Text'.
textJsonFormat :: JsonFormat e Text
textJsonFormat = abeJsonFormat ABE.asText

$makeTupleFormats

$makeNamedTupleFormats

-- |'JsonFormat' for '()' which maps to an empty array.
unitJsonFormat :: JsonFormat e ()
unitJsonFormat = aesonJsonFormat

-- |'JsonFormat' for 'Vector's which map to an array.
vectorJsonFormat :: JsonFormat e a -> JsonFormat e (V.Vector a)
vectorJsonFormat (JsonFormat (JsonProfunctor oA iA)) =
  JsonFormat (JsonProfunctor o i)
  where
    o = Aeson.Array . fmap oA
    i = V.fromList <$> ABE.eachInArray iA

-- |'JsonFormat' for 'Version' which maps to a string.
versionJsonFormat :: JsonFormat e Version
versionJsonFormat = aesonJsonFormat
