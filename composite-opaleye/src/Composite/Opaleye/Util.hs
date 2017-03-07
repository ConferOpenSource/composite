module Composite.Opaleye.Util where

import Data.Profunctor (dimap)
import Opaleye (Column, Constant, unsafeCoerceColumn)

constantColumnUsing :: Constant haskell (Column pgType)
                    -> (haskell' -> haskell)
                    -> Constant haskell' (Column pgType')
constantColumnUsing oldConstant f = dimap f unsafeCoerceColumn oldConstant
