module Composite.TH
  ( withProxies
  , withLensesAndProxies
  , withPrismsAndProxies
  , withOpticsAndProxies
  ) where

import Composite.CoRecord (Field, fieldValPrism)
import Composite.Record ((:->), Record, rlens)
import Control.Lens (Prism', _1, _head, each, over, toListOf)
import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (RecApplicative)
import Data.Vinyl.Lens (type (∈))
import Language.Haskell.TH
  ( Q, newName, mkName, nameBase
  , Body(NormalB), cxt, Dec(PragmaD, SigD, ValD), Exp(VarE), Inline(Inlinable), Name, Pat(VarP), Phases(AllPhases), Pragma(InlineP), RuleMatch(FunLike)
  , Type(AppT, ConT, ForallT, VarT), TyVarBndr(PlainTV, KindedTV), varT
  )
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
        [ PragmaD (InlineP proxyName Inlinable FunLike AllPhases)
        , SigD proxyName proxyType
        , ValD (VarP proxyName) (NormalB proxyVal) []
        ]

-- |Make 'rlens' and 'Proxy' definitions for each of the @type@ synonyms in the given block of declarations. The lenses have the same names as the synonyms
-- but with the first letter lowercased. The proxies have that name but with @_@ suffix.
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
--   fFoo :: FFoo ∈ rs => Lens' (Record rs) Int
--   fFoo = rlens fFoo_
--   fFoo_ :: Proxy FFoo
--   fFoo_ = Proxy
-- @
--
-- __Note:__ the trailing @|]@ of the quasi quote bracket has to be indented or a parse error will occur.
--
-- This is equivalent to 'withOpticsAndProxies' but without the prisms.
withLensesAndProxies :: Q [Dec] -> Q [Dec]
withLensesAndProxies = withBoilerplate True False

-- |Make 'fieldValPrism' and 'Proxy' definitions for each of the @type@ synonyms in the given block of declarations. The prisms have the same names as the
-- synonyms but prefixed with @_@. The proxies will have the same name as the synonym but with the first character lowercased and @_@ appended.
--
-- For example:
--
-- @
--   withPrismsAndProxies [d|
--     type FFoo = "foo" :-> Int
--     |]
-- @
--
-- Is equivalent to:
--
-- @
--   type FFoo = "foo" :-> Int
--   _FFoo :: FFoo ∈ rs => Prism' (Field rs) Int
--   _FFoo = fieldValPrism fFoo_
--   fFoo_ :: Proxy FFoo
--   fFoo_ = Proxy
-- @
--
-- __Note:__ the trailing @|]@ of the quasi quote bracket has to be indented or a parse error will occur.
--
-- This is equivalent to 'withOpticsAndProxies' but without the prisms.
withPrismsAndProxies :: Q [Dec] -> Q [Dec]
withPrismsAndProxies = withBoilerplate False True

-- |Make 'rlens', 'fieldValPrism', and 'Proxy' definitions for each of the @type@ synonyms in the given block of declarations.
-- The lenses have the same names as the synonyms but with the first letter lowercased, e.g. @FFoo@ becomes @fFoo@.
-- The prisms have the same names as the synonyms but with @_@ prepended, e.g. @FFoo@ becomes @_FFoo@.
-- The proxies have the same names as the synonyms but with the first letter lowercase and trailing @_@, e.g. @FFoo@ becomes @fFoo_@.
--
-- For example:
--
-- @
--   withOpticsAndProxies [d|
--     type FFoo = "foo" :-> Int
--     |]
-- @
--
-- Is equivalent to:
--
-- @
--   type FFoo = "foo" :-> Int
--   fFoo :: FFoo ∈ rs => Lens' (Record rs) Int
--   fFoo = rlens fFoo_
--   _FFoo :: FFoo ∈ rs => Prism' (Field rs) Int
--   _FFoo = fieldValPrism fFoo_
--   fFoo_ :: Proxy FFoo
--   fFoo_ = Proxy
-- @
--
-- __Note:__ the trailing @|]@ of the quasi quote bracket has to be indented or a parse error will occur.
withOpticsAndProxies :: Q [Dec] -> Q [Dec]
withOpticsAndProxies = withBoilerplate True True

data FieldDec = FieldDec
  { fieldName        :: Name
  , fieldBinders     :: [TyVarBndr]
  , fieldTypeApplied :: Type
  , fieldValueType   :: Type
  }
-- |TH splice which implements 'withLensesAndProxies', 'withPrismsAndProxies', and 'withOpticsAndProxies'
withBoilerplate :: Bool -> Bool -> Q [Dec] -> Q [Dec]
withBoilerplate generateLenses generatePrisms qDecs = do
  decs <- qDecs

  let fieldDecs = catMaybes . map fieldDecMay . toListOf (each . _TySynD) $ decs

  proxyDecs <- traverse proxyDecFor fieldDecs
  lensDecs  <- if generateLenses then traverse lensDecFor  fieldDecs else pure []
  prismDecs <- if generatePrisms then traverse prismDecFor fieldDecs else pure []

  pure $ decs <> concat proxyDecs <> concat lensDecs <> concat prismDecs

fieldDecMay :: (Name, [TyVarBndr], Type) -> Maybe FieldDec
fieldDecMay (fieldName, fieldBinders, ty) = case ty of
  AppT (AppT (ConT n) _) fieldValueType | n == ''(:->) ->
    let fieldTypeApplied         = foldl' AppT (ConT fieldName) (map binderTy fieldBinders)
        binderTy (PlainTV n')    = VarT n'
        binderTy (KindedTV n' _) = VarT n'
    in Just $ FieldDec {..}
  _ ->
    Nothing

lensNameFor, prismNameFor, proxyNameFor :: Name -> Name
lensNameFor  = mkName . over _head toLower . nameBase
prismNameFor = mkName . ("_" ++) . nameBase
proxyNameFor = mkName . (++ "_") . over _head toLower . nameBase

proxyDecFor :: FieldDec -> Q [Dec]
proxyDecFor (FieldDec { fieldName, fieldTypeApplied }) = do
  let proxyName = proxyNameFor fieldName

  proxyType <- [t|Proxy $(pure fieldTypeApplied)|]
  proxyVal <- [|Proxy|]
  pure
    [ PragmaD (InlineP proxyName Inlinable FunLike AllPhases)
    , SigD proxyName proxyType
    , ValD (VarP proxyName) (NormalB proxyVal) []
    ]

lensDecFor :: FieldDec -> Q [Dec]
lensDecFor (FieldDec {..}) = do
  f  <- newName "f"
  rs <- newName "rs"

  let fTy                     = varT f
      rsTy                    = varT rs
      proxyName               = proxyNameFor fieldName
      lensName                = lensNameFor fieldName
      proxyVal                = VarE proxyName
      lensBinders             = fieldBinders ++ [PlainTV f, PlainTV rs]

  lensContext <- cxt [ [t| Functor $fTy |], [t| $(pure fieldTypeApplied) ∈ $rsTy |] ]
  lensType    <- [t| ($(pure fieldValueType) -> $fTy $(pure fieldValueType)) -> (Record $rsTy -> $fTy (Record $rsTy)) |]
  rlensVal    <- [| rlens $(pure proxyVal) |]

  pure
    [ PragmaD (InlineP lensName Inlinable FunLike AllPhases)
    , SigD lensName (ForallT lensBinders lensContext lensType)
    , ValD (VarP lensName) (NormalB rlensVal) []
    ]

prismDecFor :: FieldDec -> Q [Dec]
prismDecFor (FieldDec {..}) = do
  rs <- newName "rs"

  let rsTy                    = varT rs
      proxyName               = proxyNameFor fieldName
      prismName               = prismNameFor fieldName
      proxyVal                = VarE proxyName
      prismBinders            = fieldBinders ++ [PlainTV rs]

  prismContext  <- cxt [ [t| RecApplicative $rsTy |], [t| $(pure fieldTypeApplied) ∈ $rsTy |] ]
  prismType     <- [t| Prism' (Field $rsTy) $(pure fieldValueType) |]
  fieldPrismVal <- [| fieldValPrism $(pure proxyVal) |]

  pure
    [ PragmaD (InlineP prismName Inlinable FunLike AllPhases)
    , SigD prismName (ForallT prismBinders prismContext prismType)
    , ValD (VarP prismName) (NormalB fieldPrismVal) []
    ]
