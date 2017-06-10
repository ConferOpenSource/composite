module Composite.Aeson.Base
  ( ToJson(..), FromJson(..), JsonProfunctor(..), _JsonProfunctor, JsonFormat(..)
  , toJsonWithFormat, fromJsonWithFormat, parseJsonWithFormat, parseJsonWithFormat'
  , dimapJsonFormat, jsonFormatWithIso, wrapJsonFormat, jsonFormatWithoutCustomError, wrappedJsonFormat
  ) where

import Control.Lens (AnIso', Iso, _2, Wrapped(type Unwrapped), _Wrapped', _Wrapped, iso, over, withIso)
import Control.Lens.TH (makeWrapped)
import Control.Monad.Except (withExceptT)
import Control.Monad.Morph (hoist)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.Aeson.BetterErrors.Internal as ABEI
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

-- |Given a @'JsonFormat' e a@ and a pair of functions @b -> a@ and @a -> Either e b@, produce a @'JsonFormat' e b@.
--
-- This is for the common case of a @newtype@ wrapper which asserts some kind of validation has been done, e.g.:
--
-- @
--   newtype MyType = MyType { unMyType :: Int }
--
--   mkMyType :: Int -> Either Text MyType
--   mkMyType i | i <= 0    = Left "must be positive!"
--              | otherwise = Right (MyType i)
--
--   myTypeJsonFormat :: JsonFormat e MyType
--   myTypeJsonFormat = wrapJsonFormat intJsonFormat mkMyType unMyType
-- @
wrapJsonFormat :: JsonFormat e a -> (a -> Either e b) -> (b -> a) -> JsonFormat e b
wrapJsonFormat (JsonFormat (JsonProfunctor oa ia)) ab ba = JsonFormat (JsonProfunctor ob ib)
  where
    ob = oa . ba
    ib = either ABE.throwCustomError pure . ab =<< ia

-- |Take a 'JsonFormat' which produces some 'Show'-able custom error and convert any custom errors into Aeson 'fail' style errors. Since the custom errors
-- are never generated by the resulting 'JsonFormat', any custom error type can be assumed.
--
-- This is commonly used to take a more specific @'JsonFormat' MyError MyType@ and make it a more generic @'JsonFormat' e MyType@, e.g. to be used as a 
-- 'Composite.Aeson.Default.defaultJsonFormat'.
jsonFormatWithoutCustomError :: Show e => JsonFormat e a -> JsonFormat e' a
jsonFormatWithoutCustomError =
  over (_Wrapped . _JsonProfunctor . _2 . _Wrapped) $
    ABEI.mapParseT $ hoist $ withExceptT $ \ case
        ABEI.BadSchema pos (ABEI.KeyMissing k)       -> ABEI.BadSchema pos (ABEI.KeyMissing k)
        ABEI.BadSchema pos (ABEI.OutOfBounds i)      -> ABEI.BadSchema pos (ABEI.OutOfBounds i)
        ABEI.BadSchema pos (ABEI.WrongType t v)      -> ABEI.BadSchema pos (ABEI.WrongType t v)
        ABEI.BadSchema pos (ABEI.ExpectedIntegral d) -> ABEI.BadSchema pos (ABEI.ExpectedIntegral d)
        ABEI.BadSchema pos (ABEI.FromAeson e)        -> ABEI.BadSchema pos (ABEI.FromAeson e)
        ABEI.BadSchema pos (ABEI.CustomError e)      -> ABEI.BadSchema pos (ABEI.FromAeson (show e))

        ABEI.InvalidJSON msg -> ABEI.InvalidJSON msg

-- |Wrap a 'JsonFormat' for type @a@ in an isomorphism to produce a new @JsonFormat@ for @b@.
jsonFormatWithIso :: AnIso' b a -> JsonFormat e a -> JsonFormat e b
jsonFormatWithIso i = withIso i dimapJsonFormat

-- |Given a format for the value type inside some wrapper type @a@ which instances 'Wrapped', produce a format which works on the wrapper type.
wrappedJsonFormat :: Wrapped a => JsonFormat e (Unwrapped a) -> JsonFormat e a
wrappedJsonFormat = jsonFormatWithIso _Wrapped'
