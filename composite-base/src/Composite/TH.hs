module Composite.TH where

import BasicPrelude
import Composite.Record (Record, rlens)
import Control.Lens (_1, _head, each, over, toListOf)
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl.Lens (RElem)
import Data.Vinyl.TypeLevel (RIndex)
import Language.Haskell.TH (Q, Body(NormalB), Dec(SigD, ValD), Exp(VarE), Name, Pat(VarP), Type(AppT, ConT), TyVarBndr, mkName, nameBase)
import Language.Haskell.TH.Lens (_TySynD)

-- |Make 'Proxy' definitions for each of the @type@ synonyms in the given block of declarations. The proxies have the same names as the synonyms but with
-- the first letter lowercased.
--
-- For example:
--
-- @
--   withProxies [d|
--     type FFoo = "foo" :-> Int
--     |]
-- @
--
-- Is equivalent to:
--
-- @
--   type FFoo = "foo" :-> Int
--   fFoo :: Proxy FFoo
--   fFoo = Proxy
-- @
--
-- __Note:__ the trailing @|]@ of the quasi quote bracket has to be indented or a parse error will occur.
withProxies :: Q [Dec] -> Q [Dec]
withProxies qDecs = do
  decs <- qDecs
  proxyDecs <- traverse proxyDecForName (toListOf (each . _TySynD . _1) decs)
  pure $ decs <> concat proxyDecs
  where
    proxyDecForName tySynName = do
      let tySynType = pure $ ConT tySynName
          proxyName = mkName . over _head toLower . nameBase $ tySynName
      proxyType <- [t|Proxy $tySynType|]
      proxyVal <- [|Proxy|]
      pure
        [ SigD proxyName proxyType
        , ValD (VarP proxyName) (NormalB proxyVal) []
        ]

-- |Make 'rlens' and 'Proxy' definitions for each of the @type@ synonyms in the given block of declarations. The lenses have the same names as the synonyms
-- but with the first letter lowercased. The proxies have that name but with _ suffix.
--
-- For example:
--
-- @
--   withLensesAndProxies [d|
--     type FFoo = "foo" :-> Int
--     |]
-- @
--
-- Is equivalent to:
--
-- @
--   type FFoo = "foo" :-> Int
--   fFoo :: RElem FFoo rs (RIndex FFoo rs) => Lens' (Record rs) Int
--   fFoo = rlens fFoo_
--   fFoo_ :: Proxy FFoo
--   fFoo_ = Proxy
-- @
--
-- __Note:__ the trailing @|]@ of the quasi quote bracket has to be indented or a parse error will occur.
withLensesAndProxies :: Q [Dec] -> Q [Dec]
withLensesAndProxies qDecs = do
  decs <- qDecs
  proxyDecs <- traverse proxyDecForName $ toListOf (each . _TySynD . _1) decs
  lensDecs <- traverse lensDecForName $ toListOf (each . _TySynD) decs
  pure $ decs <> concat proxyDecs <> concat lensDecs
  where
    proxyNameForTypeName = mkName . (++ "_") . over _head toLower . nameBase

    proxyDecForName :: Name -> Q [Dec]
    proxyDecForName tySynName = do
      let tySynType = pure $ ConT tySynName
          proxyName = proxyNameForTypeName tySynName
      proxyType <- [t|Proxy $tySynType|]
      proxyVal <- [|Proxy|]
      pure
        [ SigD proxyName proxyType
        , ValD (VarP proxyName) (NormalB proxyVal) []
        ]

    lensDecForName :: (Name, [TyVarBndr], Type) -> Q [Dec]
    lensDecForName (tySynName, _, AppT (AppT (ConT (nameBase -> ":->")) _) valTy) = do -- FIXME stop doing name hacks
      let tySynType = pure $ ConT tySynName
          proxyName = proxyNameForTypeName tySynName
          proxyVal  = VarE proxyName
          lensName  = mkName . over _head toLower . nameBase $ tySynName
      lensType <- [t|forall f rs. (Functor f, RElem $tySynType rs (RIndex $tySynType rs)) => ($(pure valTy) -> f $(pure valTy)) -> Record rs -> f (Record rs)|]
      rlensVal <- [|rlens $(pure proxyVal)|]
      pure
        [ SigD lensName lensType
        , ValD (VarP lensName) (NormalB rlensVal) [] ]
    lensDecForName (tySynName, _, _) =
      fail $ "Can only make lenses and proxies for type synonyms like type FField = \"field\" :-> Type, but " <> nameBase tySynName <> " has some other form of type"
