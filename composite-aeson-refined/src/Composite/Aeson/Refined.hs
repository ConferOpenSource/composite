{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Composite.Aeson.Refined (refinedJsonFormat) where

import Composite.Aeson (DefaultJsonFormat, defaultJsonFormat, JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor))
import Control.Monad.Error.Class (throwError)
import qualified Data.Aeson.BetterErrors as ABE
import Refined
  ( Predicate, Refined, refine, unrefine
#if MIN_VERSION_refined(0,2,0)
  , displayRefineException
#endif
  )

-- |Given a @'JsonFormat' e a@, produce a @JsonFormat e ('Refined' p a)@ where @p@ is some 'Predicate' from the refined library for @a@.
--
-- This maps to the same JSON as the given 'JsonFormat', but when parsing it will apply 'refine' to assert that the incoming JSON value conforms to the
-- predicate, failing to parse if not.
refinedJsonFormat :: Predicate p a => JsonFormat e a -> JsonFormat e (Refined p a)
refinedJsonFormat (JsonFormat (JsonProfunctor oa ia)) = JsonFormat $ JsonProfunctor o i
  where
    o = oa . unrefine
#if MIN_VERSION_refined(0,2,0)
    i = either (toss . show . displayRefineException) pure . refine =<< ia
#else
    i = either toss pure . refine =<< ia
#endif
    toss = throwError . ABE.BadSchema [] . ABE.FromAeson

instance (DefaultJsonFormat a, Predicate p a) => DefaultJsonFormat (Refined p a) where
  defaultJsonFormat = refinedJsonFormat defaultJsonFormat
