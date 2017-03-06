module Composite.TH where

import BasicPrelude
import Control.Lens (_1, _head, each, over, toListOf)
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Language.Haskell.TH (Q, Body(NormalB), Dec(SigD, ValD), Pat(VarP), Type(ConT), mkName, nameBase)
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
