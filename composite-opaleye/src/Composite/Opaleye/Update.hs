-- |Module which provides utilities for processing updates using Opaleye and Composite
module Composite.Opaleye.Update
  ( RecordToUpdate, recordToUpdate
  ) where

import Composite.Record ((:->)(Val), Rec((:&), RNil), Record)
import Data.Functor.Identity (Identity(Identity))

-- |Typeclass which allows transformation of a record from its select form to neutral update form, which boils down to wrapping fields that have defaults
-- with 'Just'.
class RecordToUpdate (rs :: [*]) (ss :: [*]) where
  -- |Transform a @'Record' rs@ obtained from the database to a @'Record' ss@ representing an updated version of the row.
  --
  -- Opaleye's @runUpdate@ family of functions all take an update function of the type @columnsR -> columnsW@, which this function implements generically
  -- for a no-op update.
  --
  -- Typically this function is composed with one or more lens @set@s which update the fields after the transformation.
  recordToUpdate :: Record rs -> Record ss

-- |For an empty record, just act as 'id'.
instance RecordToUpdate '[] '[] where
  recordToUpdate RNil = RNil
  {-# INLINE recordToUpdate #-}

-- |For a field whose type doesn't change between selection and update, just pass the field unchanged and then recurse.
instance RecordToUpdate rs ss => RecordToUpdate (r ': rs) (r ': ss) where
  recordToUpdate (r :& rs) = r :& recordToUpdate rs
  {-# INLINE recordToUpdate #-}

-- |For a field whose type at selection is @s :-> a@ but at update is @s :-> Maybe a@ (a field which has a default value) add in a 'Just' and recurse.
instance RecordToUpdate rs ss => RecordToUpdate (s :-> a ': rs) (s :-> Maybe a ': ss) where
  recordToUpdate (Identity (Val a) :& rs) = Identity (Val (Just a)) :& recordToUpdate rs
  {-# INLINE recordToUpdate #-}

