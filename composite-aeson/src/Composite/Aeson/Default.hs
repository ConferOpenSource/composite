module Composite.Aeson.Default where

import BasicPrelude
import Composite.Aeson.Base (JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor))
import Data.Aeson (FromJSON, ToJSON(toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import Data.Int (Int8, Int16)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Word (Word8, Word16)

-- |Class for associating a default JSON format with a type.
--
-- DO NOT use this as the primary interface. It should only be used for defaulting in contexts where an explicit choice can also be used.
class DefaultJsonFormat a where
  -- |Produce the default 'JsonFormat' for type @a@, which must not produce any custom errors.
  defaultJsonFormat :: JsonFormat e a

instance DefaultJsonFormat Bool         where defaultJsonFormat = boolJsonFormat
instance DefaultJsonFormat Char         where defaultJsonFormat = charJsonFormat
instance DefaultJsonFormat Scientific   where defaultJsonFormat = scientificJsonFormat
instance DefaultJsonFormat Float        where defaultJsonFormat = realFloatJsonFormat
instance DefaultJsonFormat Double       where defaultJsonFormat = realFloatJsonFormat
instance DefaultJsonFormat Int          where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int8         where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int16        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int32        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Int64        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word8        where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word16       where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word32       where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat Word64       where defaultJsonFormat = integralJsonFormat
instance DefaultJsonFormat String       where defaultJsonFormat = stringJsonFormat
instance DefaultJsonFormat Text         where defaultJsonFormat = textJsonFormat
instance DefaultJsonFormat Aeson.Value  where defaultJsonFormat = aesonValueJsonFormat
instance DefaultJsonFormat Aeson.Object where defaultJsonFormat = aesonObjectJsonFormat
instance DefaultJsonFormat Aeson.Array  where defaultJsonFormat = aesonArrayJsonFormat

-- |Produce an explicit 'JsonFormat' by using the implicit Aeson 'FromJSON' and 'ToJSON' instances.
--
-- If an @aeson-better-errors@ parser is available for @a@, it's probably better to use 'abeJsonFormat' to get the enhanced error reporting.
aesonJsonFormat :: (ToJSON a, FromJSON a) => JsonFormat e a
aesonJsonFormat = JsonFormat $ JsonProfunctor toJSON ABE.fromAesonParser

-- |Produce an explicit 'JsonFormat' by using the implicit Aeson 'ToJSON' instance and an explicit @aeson-better-errors@ 'ABE.Parse'.
abeJsonFormat :: ToJSON a => ABE.Parse e a -> JsonFormat e a
abeJsonFormat p = JsonFormat $ JsonProfunctor toJSON p

-- |'JsonFormat' for 'Bool', mapping to a JSON boolean.
boolJsonFormat :: JsonFormat e Bool
boolJsonFormat = abeJsonFormat ABE.asBool

-- |'JsonFormat' for 'Char', mapping to a JSON string.
charJsonFormat :: JsonFormat e Char
charJsonFormat = aesonJsonFormat

-- |'JsonFormat' for 'Scientific', mapping to a JSON number.
--
-- __Warning:__ some JSON parsing libraries do not accept the scientific number notation even though it's part of the JSON standard, and this format
-- uses 'Data.ByteString.Builder.Scientific.scientificBuilder' transitively which encodes very small (< 0.1) and large (> 9,999,999.0) fractional numbers
-- using scientific notation.
scientificJsonFormat :: JsonFormat e Scientific
scientificJsonFormat = abeJsonFormat ABE.asScientific

-- |Convert some 'RealFloat' value to 'Aeson.Value'. Copied from Aeson internals which do not export it.
realFloatToJson :: RealFloat a => a -> Aeson.Value
realFloatToJson d
  | isNaN d || isInfinite d = Aeson.Null
  | otherwise = Aeson.Number $ Scientific.fromFloatDigits d
{-# INLINE realFloatToJson #-}

-- |Polymorphic JSON format for any type which instances 'RealFloat'. See warning in documentation for 'scientificJsonFormat' about scientific notation.
realFloatJsonFormat :: RealFloat a => JsonFormat e a
realFloatJsonFormat = JsonFormat $ JsonProfunctor realFloatToJson ABE.asRealFloat

-- |Polymorphic JSON format for any type which instances 'Integral'.
integralJsonFormat :: Integral a => JsonFormat e a
integralJsonFormat = JsonFormat $ JsonProfunctor (Aeson.Number . fromIntegral) ABE.asIntegral

-- |JSON format for 'String'.
stringJsonFormat :: JsonFormat e String
stringJsonFormat = abeJsonFormat ABE.asString

-- |JSON format for 'Text'.
textJsonFormat :: JsonFormat e Text
textJsonFormat = abeJsonFormat ABE.asText

-- |JSON format for '()' which maps to JSON as @null@.
nullJsonFormat :: JsonFormat e ()
nullJsonFormat = abeJsonFormat ABE.asNull

-- |JSON format which does no parsing or encoding.
aesonValueJsonFormat :: JsonFormat e Aeson.Value
aesonValueJsonFormat = abeJsonFormat ABE.asValue

-- |JSON format for 'Aeson.Object' which maps to any object in JSON.
aesonObjectJsonFormat :: JsonFormat e Aeson.Object
aesonObjectJsonFormat = abeJsonFormat ABE.asObject

-- |JSON format for 'Aeson.Array' which maps to any array in JSON.
aesonArrayJsonFormat :: JsonFormat e Aeson.Array
aesonArrayJsonFormat = abeJsonFormat ABE.asArray

-- FIXME all the rest!
