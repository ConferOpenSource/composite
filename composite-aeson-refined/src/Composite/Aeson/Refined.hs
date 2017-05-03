{-# OPTIONS_GHC -fno-warn-orphans #-}
module Composite.Aeson.Refined (refinedJsonFormat) where

import Composite.Aeson (DefaultJsonFormat, defaultJsonFormat, JsonFormat(JsonFormat), JsonProfunctor(JsonProfunctor))
import Refined (Predicate, Refined, refine, unrefine)

-- |Given a @'JsonFormat' e a@, produce a @JsonFormat e ('Refined' p a)@ where @p@ is some 'Predicate' from the refined library for @a@.
--
-- This maps to the same JSON as the given 'JsonFormat', but when parsing it will apply 'refine' to assert that the incoming JSON value conforms to the
-- predicate, failing to parse if not.
refinedJsonFormat :: Predicate p a => JsonFormat e a -> JsonFormat e (Refined p a)
refinedJsonFormat (JsonFormat (JsonProfunctor oa ia)) = JsonFormat $ JsonProfunctor o i
  where
    o = oa . unrefine
    i = either fail pure . refine =<< ia

instance (DefaultJsonFormat a, Predicate p a) => DefaultJsonFormat (Refined p a) where
  defaultJsonFormat = refinedJsonFormat defaultJsonFormat
