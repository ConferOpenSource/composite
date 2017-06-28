module Composite.Aeson.Enum where

import Control.Monad.Error.Class (throwError)
import Composite.Aeson.Base (JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic(type Rep))
import Generics.Deriving.ConNames (ConNames, conNames)
import Generics.Deriving.Enum (Enum', genumDefault)

-- |For some type @a@ which represents an enumeration (i.e. all nullary constructors) generate a 'JsonFormat' which maps that type to strings in JSON.
--
-- Each constructor will be mapped to a string with the same value as its name with some prefix removed.
--
-- For example, given:
--
-- > data MyEnum = MyEnumFoo | MyEnumBar
-- > myEnumFormat :: JsonFormat e MyEnum
-- > myEnumFormat = enumJsonFormat "MyEnum"
--
-- Then:
--
-- > toJsonWithFormat myEnumFormat MyEnumFoo == Aeson.String "Foo"
enumJsonFormat :: forall e a. (Show a, Ord a, Generic a, ConNames (Rep a), Enum' (Rep a)) => String -> JsonFormat e a
enumJsonFormat prefix =
  let names = map (pack . removePrefix) $ conNames (undefined :: a)
      removePrefix s
        | Just suffix <- stripPrefix prefix s = suffix
        | otherwise                           = s
      values = genumDefault
      lookupText  = flip HM.lookup . HM.fromList $ zip names values
      lookupValue = flip  M.lookup .  M.fromList $ zip values names
      expectedValues = "one of " ++ (intercalate ", " . map unpack $ names)
  in enumMapJsonFormat lookupText lookupValue expectedValues

-- |For some type @a@ which bidirectional mapping functions can be provided, produce a 'JsonFormat' which maps to JSON strings.
enumMapJsonFormat :: Show a => (Text -> Maybe a) -> (a -> Maybe Text) -> String -> JsonFormat e a
enumMapJsonFormat lookupText lookupValue expectedText = JsonFormat $ JsonProfunctor toJson fromJson
  where
    toJson a =
      case lookupValue a of
        Nothing -> error $ "unrecognized enum value " ++ show a -- eugh
        Just t  -> Aeson.String t

    fromJson = do
      t <- ABE.asText
      case lookupText t of
        Nothing -> throwError $ ABE.BadSchema [] $ ABE.FromAeson $
                     "expected " ++ expectedText ++ ", not " ++ unpack t
        Just v  -> pure v
  
