module Composite.Opaleye.Util where

import Data.Profunctor (dimap)
import Opaleye (Column, Constant, unsafeCoerceColumn)

-- |Coerce one type of 'Column' 'Constant' profunctor to another using by just asserting the changed type on the column side and using the given function
-- on the Haskell side. Useful when the PG value representation is the same but the Haskell type changes, e.g. for enums.
constantColumnUsing :: Constant haskell (Column pgType)
                    -> (haskell' -> haskell)
                    -> Constant haskell' (Column pgType')
constantColumnUsing oldConstant f = dimap f unsafeCoerceColumn oldConstant
