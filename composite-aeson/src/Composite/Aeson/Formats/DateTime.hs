module Composite.Aeson.Formats.DateTime
  ( DateTimeFormat(..), regularDateTimeFormat
  , dateTimeJsonFormat
  , iso8601DateJsonFormat, iso8601DateTimeJsonFormat, iso8601TimeJsonFormat
  ) where

import Composite.Aeson.Base (JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor))
import Composite.Aeson.DateTimeFormatUtils (fixupTzIn, fixupTzOut, fixupMs)
import Composite.Aeson.Formats.Provided (stringJsonFormat)
import Data.Either (partitionEithers)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime, ParseTime, TimeLocale, defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (TimeOfDay)

-- |Structure carrying the date/time format string along with an example for error messaging and functions which optionally permute the input or output
-- before using the format.
data DateTimeFormat = DateTimeFormat
  { dateTimeFormat           :: String
  , dateTimeFormatExample    :: String
  , dateTimeFormatPreParse   :: String -> String
  , dateTimeFormatPostFormat :: String -> String
  }

-- |Construct a 'DateTimeFormat' with no pre- or post- processing.
regularDateTimeFormat :: String -> String -> DateTimeFormat
regularDateTimeFormat format example = DateTimeFormat format example id id

-- |'JsonFormat' for any type which 'ParseTime' and 'FormatTime' are defined for which maps to JSON via the first format given and maps from JSON via
-- any format given.
dateTimeJsonFormat :: (ParseTime t, FormatTime t) => TimeLocale -> NonEmpty DateTimeFormat -> JsonFormat e t
dateTimeJsonFormat locale formats@(outFormat :| otherInFormats) = JsonFormat (JsonProfunctor dayOut dayIn)
  where
    formatsList = NEL.toList formats
    JsonFormat (JsonProfunctor stringOut stringIn) = stringJsonFormat
    dayOut = stringOut . dateTimeFormatPostFormat outFormat . formatTime locale (dateTimeFormat outFormat)
    dayIn = do
      s <- stringIn
      let attempt format = successOrFail Left Right $ parseTimeM True locale (dateTimeFormat format) (dateTimeFormatPreParse format s)
          attempts = map attempt formatsList
      case partitionEithers attempts of
        (_, a : _) ->
          pure a
        (es, _) | null otherInFormats ->
          fail $ "expected date/time string formatted as " <> dateTimeFormatExample outFormat <> ", but: " <> intercalate ", " es
        (es, _) ->
          fail $ "expected date/time string formatted as one of "
              <> intercalate ", " (map dateTimeFormatExample formatsList)
              <> ", but: " <> intercalate ", " es

-- |ISO8601 extended date format (@yyyy-mm-dd@).
iso8601DateJsonFormat :: JsonFormat e Day
iso8601DateJsonFormat =
  dateTimeJsonFormat defaultTimeLocale (fmt :| [])
  where
    fmt = regularDateTimeFormat "%F" "yyyy-mm-dd"

-- |ISO8601 extended date/time format (@yyyy-mm-ddThh:mm:ss.sssZ@ or @yyyy-mm-ttThh:mm:ssZ@)
iso8601DateTimeJsonFormat :: JsonFormat e UTCTime
iso8601DateTimeJsonFormat =
  dateTimeJsonFormat defaultTimeLocale (withMs :| [withoutMs])
  where
    withMs    = DateTimeFormat "%FT%T.%Q%z" "yyyy-mm-ddThh:mm:ss.sssZ" fixupTzIn (fixupTzOut . fixupMs)
    withoutMs = DateTimeFormat "%FT%T%z"    "yyyy-mm-ddThh:mm:ssZ"     fixupTzIn fixupTzOut

-- |ISO8601 extended time format (@hh:mm:ss.sss@ or @hh:mm:ss@)
iso8601TimeJsonFormat :: JsonFormat e TimeOfDay
iso8601TimeJsonFormat =
  dateTimeJsonFormat defaultTimeLocale (withMs :| [withoutMs])
  where
    withMs    = DateTimeFormat "%T.%Q%z" "hh:mm:ss.sss" id fixupMs
    withoutMs = DateTimeFormat "%T.%Q"   "hh:mm:ss"     id id


-- |Monad for capturing uses of 'fail', because @Data.Time.Format@ has a poorly factored interface.
data SuccessOrFail a = Fail String | Success a

instance Functor SuccessOrFail where
  fmap f (Success a) = Success (f a)
  fmap _ (Fail    f) = Fail    f

instance Applicative SuccessOrFail where
  pure   = Success
  Success f <*> Success a = Success (f a)
  Success _ <*> Fail    f = Fail    f
  Fail    f <*> _         = Fail    f

instance Monad SuccessOrFail where
  return = Success
  fail   = Fail

  Success a >>= k = k a
  Fail    f >>= _ = Fail f

-- |Evaluate some action of type @Monad m => m a@ and apply either the first or second function based on whether the computation completed or used @fail@.
successOrFail :: (String -> b) -> (a -> b) -> (forall m. Monad m => m a) -> b
successOrFail _ f (Success a) = f a
successOrFail f _ (Fail    s) = f s


