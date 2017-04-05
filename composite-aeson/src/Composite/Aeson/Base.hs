module Composite.Aeson.Base
  ( ToJson(..), FromJson(..), JsonProfunctor(..), _JsonProfunctor, JsonFormat(..)
  , toJsonWithFormat, fromJsonWithFormat, parseJsonWithFormat, parseJsonWithFormat'
  , dimapJsonFormat, jsonFormatWithIso, wrappedJsonFormat
  ) where

import Control.Lens (AnIso', Iso, Wrapped(type Unwrapped), _Wrapped', _Wrapped, iso, over, withIso)
import Control.Lens.TH (makeWrapped)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Profunctor (Profunctor(dimap))
import Data.Text (Text)
import Data.Void (Void)

-- |Type of functions which take a value @a@ and convert it to an 'Aeson.Value'.
--
-- Wrapper around a function of type @a -> Aeson.Value@.
--
-- Doesn't currently include support for the newer Aeson Encoding machinery, but should.
newtype ToJson a = ToJson { unToJson :: a -> Aeson.Value }

instance Contravariant ToJson where
  contramap f (ToJson g) = ToJson (g . f)

-- |Type of parsers which might emit some custom error of type @e@ and produce a value of type @a@ on success.
--
-- @a@ is the type of value that can be parsed from JSON using this profunctor, and @e@ is the type of custom error that can be produced when the JSON is
-- unacceptable. If your parser doesn't produce any custom errors, leave this type polymorphic.
--
-- Wrapper about an @aeson-better-errors@ 'ABE.Parse' @e@ @a@.
newtype FromJson e a = FromJson { unFromJson :: ABE.Parse e a }

deriving instance Functor (FromJson e)

-- |Type of profunctors which produce and consume JSON, a composition of @ToJson@ and @FromJson@.
--
-- @a@ is the type of value that can be converted to 'Aeson.Value' using this profunctor.
-- @b@ is the type of value that can be parsed from JSON using this profunctor, and @e@ is the type of custom error that can be produced when the JSON is
-- unacceptable. If your parser doesn't produce any custom errors, leave this type polymorphic.
--
-- Profunctors must have two type parameters @a@ and @b@ so this type has two, but @JsonProfunctor@s with different types aren't useful for JSON processing
-- directly. See 'JsonFormat' for a wrapper which fixes the two types.
--
-- Doesn't currently include support for the newer Aeson Encoding machinery, but should.
data JsonProfunctor e a b = JsonProfunctor (a -> Aeson.Value) (ABE.Parse e b)

instance Profunctor (JsonProfunctor e) where
  dimap f g (JsonProfunctor o i) = JsonProfunctor (o . f) (g <$> i)

-- |Observe that a 'JsonProfunctor' is isomorphic to a pair with a @ToJson@ and @FromJson@.
_JsonProfunctor :: Iso (JsonProfunctor e a b) (JsonProfunctor e' a' b') (ToJson a, FromJson e b) (ToJson a', FromJson e' b')
_JsonProfunctor =
  iso (\ (JsonProfunctor o i) -> (ToJson o, FromJson i))
      (\ (ToJson o, FromJson i) -> JsonProfunctor o i)

-- |Wrapper around 'JsonProfunctor' for use in JSON processing when the profunctor represents a bijection between JSON and a single type @a@, i.e. for
-- @JsonProfunctor e a a@.
newtype JsonFormat e a = JsonFormat { unJsonFormat :: JsonProfunctor e a a }

-- |Given a 'JsonFormat' for @a@, convert a value of @a@ into an 'Aeson.Value'.
toJsonWithFormat :: JsonFormat e a -> a -> Aeson.Value
toJsonWithFormat (JsonFormat (JsonProfunctor o _)) = o

-- |Given a 'JsonFormat' for @a@ which can produce custom errors of type @e@, yield an @aeson-better-errors@ 'ABE.Parse' which can be used to consume JSON.
fromJsonWithFormat :: JsonFormat e a -> ABE.Parse e a
fromJsonWithFormat (JsonFormat (JsonProfunctor _ i)) = i

-- |Given a 'JsonFormat' for @a@ which produces custom errors of type @e@ and some function to format those errors as messages, produce an Aeson parser function
-- @Value -> Parser a@.
parseJsonWithFormat :: (e -> Text) -> JsonFormat e a -> Aeson.Value -> Aeson.Parser a
parseJsonWithFormat showError = ABE.toAesonParser showError . fromJsonWithFormat

-- |Given a 'JsonFormat' for @a@ which doesn't produce custom errors, produce an Aeson parser function @Value -> Parser a@.
parseJsonWithFormat' :: JsonFormat Void a -> Aeson.Value -> Aeson.Parser a
parseJsonWithFormat' = ABE.toAesonParser' . fromJsonWithFormat

makeWrapped ''ToJson
makeWrapped ''FromJson
makeWrapped ''JsonFormat

-- |Wrap a 'JsonFormat' for type @a@ in a pair of functions representing an isomorphism between @a@ and @b@ to produce a new @JsonFormat@ for @b@.
dimapJsonFormat :: (b -> a) -> (a -> b) -> JsonFormat e a -> JsonFormat e b
dimapJsonFormat f g = over _Wrapped (dimap f g)

-- |Wrap a 'JsonFormat' for type @a@ in an isomorphism to produce a new @JsonFormat@ for @b@.
jsonFormatWithIso :: AnIso' b a -> JsonFormat e a -> JsonFormat e b
jsonFormatWithIso i = withIso i dimapJsonFormat

-- |Given a format for the value type inside some wrapper type @a@ which instances 'Wrapped', produce a format which works on the wrapper type.
wrappedJsonFormat :: Wrapped a => JsonFormat e (Unwrapped a) -> JsonFormat e a
wrappedJsonFormat = jsonFormatWithIso _Wrapped'
