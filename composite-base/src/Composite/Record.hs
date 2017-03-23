module Composite.Record
  ( Rec, Record, Identity(Identity)
  , pattern (:*:), pattern (:^:), pattern Nil, pattern Val
  , (:->)(Col, getCol)
  , rlens, rlens'
  ) where

import BasicPrelude
import Data.Vinyl (Rec((:&), RNil))
import qualified Data.Vinyl as Vinyl
import Data.Vinyl.Functor (Identity(Identity))
import Data.Vinyl.Lens (RElem)
import Data.Vinyl.TypeLevel (RIndex)
import Frames (Record, (:->)(Col, getCol))
import qualified Frames

-- |Pattern synonym equivalent to the empty record 'RNil'.
--
-- This pattern is bidirectional meaning you can use it either a pattern or as a constructor, e.g.
--
-- @
--   let Nil = Nil :: 'Record' '[]
-- @
--
-- is valid.
pattern Nil :: Vinyl.Rec f '[]
pattern Nil = RNil

-- |Bidirectional pattern matching the first field of a record using ':->' values and the 'Identity' functor.
--
-- This pattern is bidirectional meaning you can use it either as a pattern or a constructor, e.g.
--
-- @
--   let rec = 123 :*: Just "foo" :*: Nil
--       foo :*: bar :*: Nil = rec
-- @
--
-- Mnemonic: @*@ for products.
pattern (:*:) :: () => () => a -> Rec Identity rs -> Rec Identity (s :-> a ': rs)
pattern (:*:) a rs = Identity (Col a) :& rs
infixr 5 :*:

-- |Bidirectional pattern matching the first field of a record using ':->' values and any functor.
--
-- This pattern is bidirectional meaning you can use it either as a pattern or a constructor, e.g.
--
-- @
--   let rec = Just 123 :^: Just "foo" :^: Nil
--       Just foo :^: Just bar :^: Nil = rec
-- @
--
-- Mnemonic: @^@ for products (record) of products (functor).
pattern (:^:) :: Functor f => () => f a -> Rec f rs -> Rec f (s :-> a ': rs)
pattern (:^:) fa rs <- (map getCol -> fa) :& rs where
  (:^:) fa rs = map Col fa :& rs
infixr 5 :^:

-- |Bidirectional pattern unwrapping @Identity (s :-> a)@ to @a@.
pattern Val :: a -> Identity (s :-> a)
pattern Val a = Identity (Col a)

-- |Lens to a particular field of a record using the 'Identity' functor.
--
-- For example, given:
--
-- @
--   type FFoo = "foo" :-> Int
--   type FBar = "bar" :-> String
--   fBar_ :: Proxy FBar
--   fBar_ = Proxy
--
--   rec :: 'Rec' 'Identity' '[FFoo, FBar]
--   rec = 123 :*: "hello!" :*: Nil
-- @
--
-- Then:
--
-- @
--   view (rlens fBar_)               rec == "hello!"
--   set  (rlens fBar_) "goodbye!"    rec == 123 :*: "goodbye!" :*: Nil
--   over (rlens fBar_) (map toUpper) rec == 123 :*: "HELLO!"   :*: Nil
-- @
rlens :: (Functor g, RElem (s :-> a) rs (RIndex (s :-> a) rs), Functor g) => proxy (s :-> a) -> (a -> g a) -> Rec Identity rs -> g (Rec Identity rs)
rlens = Frames.rlens

-- |Lens to a particular field of a record using any functor.
--
-- For example, given:
--
-- @
--   type FFoo = "foo" :-> Int
--   type FBar = "bar" :-> String
--   fBar_ :: Proxy FBar
--   fBar_ = Proxy
--
--   rec :: 'Rec' 'Maybe' '[FFoo, FBar]
--   rec = Just 123 :^: Just "hello!" :^: Nil
-- @
--
-- Then:
--
-- @
--   view (rlens' fBar_)                      rec == Just "hello!"
--   set  (rlens' fBar_) Nothing              rec == Just 123 :^: Nothing       :^: Nil
--   over (rlens' fBar_) (fmap (map toUpper)) rec == Just 123 :^: Just "HELLO!" :^: Nil
-- @
rlens' :: (Functor f, Functor g, RElem (s :-> a) rs (RIndex (s :-> a) rs), Functor g) => proxy (s :-> a) -> (f a -> g (f a)) -> Rec f rs -> g (Rec f rs)
rlens' proxy f =
  Vinyl.rlens proxy $ \ (map getCol -> fa) ->
    map Col <$> f fa

