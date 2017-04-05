module Composite.Opaleye.RecordTable where

import BasicPrelude
import Composite.Record ((:->)(Val), Rec((:&), RNil))
import Data.Functor.Identity (Identity(Identity))
import Data.Profunctor (dimap)
import Data.Profunctor.Product ((***!))
import qualified Data.Profunctor.Product as PP
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Opaleye (Column, TableProperties, required, optional)

-- |Helper typeclass which picks which of 'required' or 'optional' to use for a pair of write column type and read column type.
--
-- @DefaultRecTableField (Maybe (Column a)) (Column a)@ uses 'optional'.
-- @DefaultRecTableField        (Column a)  (Column a)@ uses 'required'.
class DefaultRecTableField write read where
  defaultRecTableField :: String -> TableProperties write read

instance DefaultRecTableField (Maybe (Column a)) (Column a) where
  defaultRecTableField = optional

instance DefaultRecTableField (Column a) (Column a) where
  defaultRecTableField = required

-- |Type class for producing a default 'TableProperties' schema for some expected record types. 'required' and 'optional' are chosen automatically and the
-- column is named after the record fields, using 'NamedField' to reflect the field names.
--
-- For example, given:
--
-- >  type WriteRec = Record '["id" :-> Maybe (Column PGInt8), "name" :-> Column PGText]
-- >  type ReadRec  = Record '["id" :->        Column PGInt8 , "name" :-> Column PGText]
--
-- This:
--
-- >  defaultRecTable :: TableProperties WriteRec ReadRec
--
-- Is equivalent to:
--
-- > pReq (optional "id" &: required "name" &: Nil)
--
--
-- Alternately, use 'Composite.Opaleye.ProductProfunctors.pReq' and the usual Opaleye 'required' and 'optional'.
class DefaultRecTable write read where
  defaultRecTable :: TableProperties (Rec Identity write) (Rec Identity read)

instance DefaultRecTable '[] '[] where
  defaultRecTable = dimap (const ()) (const RNil) PP.empty

instance
    forall s r reads w writes.
    ( KnownSymbol s
    , DefaultRecTableField w r
    , DefaultRecTable writes reads
    ) => DefaultRecTable (s :-> w ': writes) (s :-> r ': reads) where
  defaultRecTable =
    dimap (\ (Identity (Val w) :& writeRs) -> (w, writeRs))
          (\ (r, readRs) -> (Identity (Val r) :& readRs))
          (step  ***! recur)
    where
      step :: TableProperties w r
      step = defaultRecTableField $ symbolVal (Proxy :: Proxy s)
      recur :: TableProperties (Rec Identity writes) (Rec Identity reads)
      recur = defaultRecTable

