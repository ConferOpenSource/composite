-- |Tools for implementing type-safe routing using corecord 'Field's.
module Composite.Reflex.Routing where

import Composite.CoRecord (CoRec(CoVal), Field, FoldRec, firstField)
import Composite.Record ((:->)(Val), getVal)
import Control.Lens (view)
import Control.Monad (mfilter, mplus)
import Data.Functor.Identity (Identity(Identity))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Vinyl (Rec, rlens, rmap)
import Web.HttpApiData (FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece))

-- |Structure of a route with type @a@, which provides how to take @a@, make a relative path out of it or parse it from a relative path.
data Route a = Route
  { routePrint :: a -> [Text]
  -- ^Format @a@ as a list of URL segments, e.g. @["customers", "22", "logistic"]@.
  , routeParse :: [Text] -> Maybe a
  -- ^Parse a list of URL segments into a @Just a@, or @Nothing@ if the URL segments don't correspond to this route.
  }

-- |Left-biased composition of two routes such that printing uses the left hand route, and parsing prefers the left hand one but tries the right hand one
-- if the left hand one parses @Nothing@.
orAccept :: Route a -> Route a -> Route a
orAccept (Route o i1) (Route _ i2) = Route o (\ segs -> i1 segs `mplus` i2 segs)

-- |Multiple parallel routes are represented as a record bounded with 'Route'
type Routes = Rec Route

-- |A single chosen path is represented by a 'Field' corecord.
type Path = Field

-- |Given multiple routes to choose from @'Routes' rs@, compose them into a single @'Route' ('Path' rs)@.
choose :: FoldRec rs rs => Routes rs -> Route (Path rs)
choose routes = Route o i
  where
    o (CoVal (Identity (a :: a))) = routePrint (view (rlens (Proxy @a)) routes) a
    i segs = firstField $ rmap (flip routeParse segs) routes

-- |Represent a single path segment @a@ using its 'FromHttpApiData' instance to parse and 'ToHttpApiData' instance to print.
capture :: (FromHttpApiData a, ToHttpApiData a) => Route b -> Route (a, b)
capture rest = Route o i
  where
    o (a, b)   = toUrlPiece a : routePrint rest b
    i (s : ss) = (,) <$> either (const Nothing) Just (parseUrlPiece s)
                     <*> routeParse rest ss
    i []       = Nothing

-- |Lift up a @'Route' a@ to a @'Route' (s :-> a)@ which just applies or removes the @:->@ constructor.
field :: Route a -> Route (s :-> a)
field (Route o i) = Route (o . getVal) (fmap Val . i)

-- |Represent the end of a path, i.e. print as @[]@ and only parse @[]@.
end :: Route (s :-> ())
end = Route (const []) (fmap (const $ Val ()) . mfilter null . Just)

-- |Represent a segment with a fixed text value.
seg :: Text -> Route a -> Route a
seg p (Route {..}) =
  Route ((p:) . routePrint) $ \ case
    (p':rest) | p' == p -> routeParse rest
    _                   -> Nothing
