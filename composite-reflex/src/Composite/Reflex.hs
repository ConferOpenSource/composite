-- |Module containing Reflex utilites for working with composite records and corecords.
module Composite.Reflex
  ( distributeRecOverDyn, dynCase
  ) where

import Composite.CoRecord (Case'(Case'), Cases', CoRec(CoVal), Field, asA, fieldToRec)
import Composite.DMap (RecDSums, dmapToRec, recToDMap)
import Composite.Record (Rec, Record)
import Control.Lens ((<&>))
import Data.Function (on)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (RecApplicative, recordToList, rmap)
import Data.Vinyl.Functor ((:.), getCompose, Const(Const))
import Data.Vinyl.Lens (rget)
import Reflex.Dom (DomBuilder, Dynamic, Event, MonadHold, PostBuild, Reflex, distributeDMapOverDynPure, dyn, fmapMaybe, holdDyn, uniqDynBy, updated)

-- FIXME recDyn version with Rec (Dynamic t :. f) once distributeDMapOverDynPure is more generalized

-- |Given a @'Rec' ('Dynamic' t) rs@, transpose into a @'Dynamic' t ('Rec' 'Maybe' rs)@ which updates any time one of the 'Dynamic's inside the input record
-- updates.
distributeRecOverDyn :: forall t rs. (Reflex t, RecDSums rs) => Rec (Dynamic t) rs -> Dynamic t (Record rs)
distributeRecOverDyn r = rmap recoverField . dmapToRec <$> distributeDMapOverDynPure (recToDMap r)
  where
    recoverField :: (Maybe :. Identity) a -> Identity a
    recoverField
      = fromMaybe (error "distributeDMapOverDynPure returned a DMap of a different shape than the input")
      . getCompose

-- |Given a @'Dynamic' t ('Field' rs)@ (which must be nonempty) case split on @rs@, passing the each value to each widget given as a subsidiary 'Dynamic'.
--
-- The intent is to push uses of 'dyn' down as far as possible, switching which DOM hierarchy is active only when the constructors change and letting each
-- hierarchy choose what to make dynamic.
--
-- For example, given @df :: 'Dynamic' t ('Field' '[Int, String])@:
--
-- @
--   dynCase df
--     $  Case' (\ di -> void $ text "it's an int: " >> dyn (display di))
--     :& Case' (\ ds -> void $ text "it's a string: " >> dyn (display ds))
--     :& RNil
-- @
--
-- In this example, which hierarchy to use would switch based on whether @df@ carried an @Int@ or @String@, but the hierarchy doesn't switch if the value
-- changes but stays on the same selector. In other words, if @df@ had @"hi!"@ at time 0, @"hello!"@ at time 1, and @123@ at time 2, the text node @it's a
-- string@ would be inserted into the DOM at time 0, remain in the DOM at time 1, and only be replaced at time 2.
--
-- @dynCase@ yields an 'Event' which fires each time @df@ changes selector (e.g. from @Int@ to @String@ or vice versa in the above example) and carries
-- the value produced by the particular case.
dynCase
  :: forall t m r rs a.
     ( Reflex t
     , DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , RecApplicative (r ': rs)
     )
  => Dynamic t (Field (r ': rs))
  -> Cases' (Dynamic t) (r ': rs) (m a)
  -> m (Event t a)
dynCase d cases = do
  let selector = uniqDynBy distinctSelector d
  dyn $ selector <&> \ (CoVal (initial :: Identity r')) -> do
    da <- holdDyn (runIdentity initial) (fmapMaybe (asA (Proxy @r')) (updated d))
    let Case' k = rget (Proxy @r') cases
    k da
  where
    distinctSelector :: Field (r ': rs) -> Field (r ': rs) -> Bool
    distinctSelector = (==) `on` (recordToList . rmap (Const . isJust) . fieldToRec)
