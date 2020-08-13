{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Composite.Record.Binary where

import Composite.Record((:->), Record, Rec((:&)), val, getVal)
import Control.Applicative(liftA2)
import Data.Binary(Binary(put, get))
import Data.Functor.Identity(runIdentity)

instance Binary a => Binary (s :-> a) where
  put = put . getVal
  get = fmap (runIdentity . val) get

instance Binary (Record '[])

instance (Binary x, Binary (Record xs)) => Binary (Record (x : xs)) where
  put (x :& xs) = put x >> put xs
  get = liftA2 (:&) get get
