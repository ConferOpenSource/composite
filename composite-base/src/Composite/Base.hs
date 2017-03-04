{-# OPTIONS_GHC -fno-warn-orphans #-}
module Composite.Base (NamedField(..), fieldAsPair) where

import BasicPrelude
import Control.Lens (Wrapped(type Unwrapped, _Wrapped'), Rewrapped, _Wrapped, iso, view)
import Data.Proxy (Proxy(Proxy))
import Data.Text (pack)
import Frames ((:->)(Col, getCol))
import GHC.TypeLits (KnownSymbol, symbolVal)

-- |Class of types which represent fields which can be named statically (i.e. via their type only) and contain some value.
class (Wrapped f, Rewrapped f f) => NamedField f where
  -- |Reflect the name of the field as @Text@ given some proxy representing the type.
  fieldName :: proxy f -> Text

-- |Extract the value and reflect the name of some named field.
fieldAsPair :: forall f. NamedField f => f -> (Text, Unwrapped f)
fieldAsPair = (fieldName (Proxy :: Proxy f),) . view _Wrapped

instance Wrapped (s :-> a) where
  type Unwrapped (s :-> a) = a
  _Wrapped' = iso getCol Col

instance (t ~ (s :-> b)) => Rewrapped (s :-> a) t
instance KnownSymbol s => NamedField (s :-> a) where
  fieldName _ = pack $ symbolVal (Proxy :: Proxy s)
