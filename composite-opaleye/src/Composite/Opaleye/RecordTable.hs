module Composite.Opaleye.RecordTable where

import Composite.Record ((:->)(Val), Rec((:&), RNil))
import Data.Functor.Identity (Identity(Identity))
import Data.Profunctor (dimap)
import Data.Profunctor.Product ((***!))
import qualified Data.Profunctor.Product as PP
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Opaleye (Field, requiredTableField, optionalTableField)
import Opaleye.Internal.Table (TableFields)

-- |Helper typeclass which picks which of 'requiredTableField' or 'optionalTableField' to use for a pair of write column type and read column type.
--
-- @DefaultRecTableField (Maybe (Field a)) (Field a)@ uses 'optionalTableField'.
-- @DefaultRecTableField        (Field a)  (Field a)@ uses 'requiredTableField'.
class DefaultRecTableField write read where
  defaultRecTableField :: String -> TableFields write read

instance DefaultRecTableField (Maybe (Field a)) (Field a) where
  defaultRecTableField = optionalTableField

instance DefaultRecTableField (Field a) (Field a) where
  defaultRecTableField = requiredTableField

-- |Type class for producing a default 'TableFields' schema for some expected record types. 'requiredTableField' and 'optionalTableField' are chosen automatically and the
-- column is named after the record fields, using 'NamedField' to reflect the field names.
--
-- For example, given:
--
-- >  type WriteRec = Record '["id" :-> Maybe (Field PGInt8), "name" :-> Field PGText]
-- >  type ReadRec  = Record '["id" :->        Field PGInt8 , "name" :-> Field PGText]
--
-- This:
--
-- >  defaultRecTable :: TableFields WriteRec ReadRec
--
-- Is equivalent to:
--
-- > pRec (optionalTableField "id" &: requiredTableField "name" &: Nil)
--
--
-- Alternately, use 'Composite.Opaleye.ProductProfunctors.pRec' and the usual Opaleye 'requiredTableField' and 'optionalTableField'.
class DefaultRecTable write read where
  defaultRecTable :: TableFields (Rec Identity write) (Rec Identity read)

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
      step :: TableFields w r
      step = defaultRecTableField $ symbolVal (Proxy :: Proxy s)
      recur :: TableFields (Rec Identity writes) (Rec Identity reads)
      recur = defaultRecTable
