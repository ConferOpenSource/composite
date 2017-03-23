module Composite.Aeson.TH
  ( makeRecJsonWrapper
  , makeRecJsonWrapperExplicit
  ) where

import BasicPrelude
import Composite.Aeson.Base (JsonFormat, dimapJsonFormat, parseJsonWithFormat', toJsonWithFormat)
import Composite.Aeson.Record (defaultJsonFormatRec, recJsonFormat)
import Control.Lens (_head, over)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (toLower)
import Frames (Record)
import Language.Haskell.TH
  ( Q, clause, cxt
  , normalB
  , recC, varBangType, bang, bangType, noSourceUnpackedness, noSourceStrictness
  , Dec, funD, instanceD, newtypeD, sigD, valD
  , Exp, conE, varE
  , Name, mkName, newName
  , varP
  , conT
  )

-- |TH splice which makes it more convenient to define ToJSON / FromJSON instances for record types.
--
-- For example:
--
-- @
--   type MyRecord = '[FFoo, FBar]
--   makeRecJsonWrapper "MyRecordJson" ''MyRecord
-- @
--
-- is equivalent to:
--
-- @
--   newtype MyRecordJson = MyRecordJson { unMyRecordJson :: Record MyRecord }
--   myRecordJsonFormat :: 'Composite.Aeson.Record.JsonFormatRec' Void MyRecordJson
--   myRecordJsonFormat =
--     'dimapJsonFormat' unMyRecordJson MyRecordJson $
--       'recJsonFormat' defaultJsonFormatRec
--   instance FromJSON MyRecordJson where
--     parseJSON = 'parseJsonWithFormat'' myRecordJsonFormat
--   instance ToJSON MyRecordJson where
--     toJSON = 'toJsonWithFormat' myRecordJsonFormat
-- @
--
-- This function uses 'defaultJsonFormatRec' to derive the formatting for the record. If you want to customize that formatting, use
-- 'makeRecJsonWrapperExplicit' instead.
makeRecJsonWrapper :: String -> Name -> Q [Dec]
makeRecJsonWrapper wrapperName tyName =
  makeRecJsonWrapperExplicit wrapperName tyName [| defaultJsonFormatRec |]

-- |TH splice which makes it more convenient to define ToJSON / FromJSON instances for record types.
--
-- For example:
--
-- @
--   type MyRecord = '[FFoo, FBar]
--   makeRecJsonWrapperExplicit "MyRecordJson" ''MyRecord [| set (rlens fFoo_) specialFormat defaultJsonFormatRec |]
-- @
--
-- is equivalent to:
--
-- @
--   newtype MyRecordJson = MyRecordJson { unMyRecordJson :: Record MyRecord }
--   myRecordJsonFormat :: 'Composite.Aeson.Record.JsonFormatRec' Void MyRecordJson
--   myRecordJsonFormat =
--     'dimapJsonFormat' unMyRecordJson MyRecordJson $
--       'recJsonFormat' (set (rlens fFoo_) specialFormat defaultJsonFormatRec)
--   instance FromJSON MyRecordJson where
--     parseJSON = 'parseJsonWithFormat'' myRecordJsonFormat
--   instance ToJSON MyRecordJson where
--     toJSON = 'toJsonWithFormat' myRecordJsonFormat
-- @
makeRecJsonWrapperExplicit :: String -> Name -> Q Exp -> Q [Dec]
makeRecJsonWrapperExplicit wrapperNameStr fieldsTyName recFormatExp = do
  let wrapperName = mkName wrapperNameStr
      extractorName = mkName $ "un" <> wrapperNameStr
      recordTy = [t| Record $(conT fieldsTyName) |]
  formatName <- newName $ over _head toLower wrapperNameStr <> "Format"
  sequence
    [ newtypeD
        (cxt [])
        wrapperName
        [] -- TyVarBndrs
        Nothing -- kind
        (recC wrapperName [varBangType extractorName (bangType (bang noSourceUnpackedness noSourceStrictness) recordTy)])
        (cxt []) -- deriving context
    , sigD
        formatName
        [t| forall e. JsonFormat e $(conT wrapperName) |]
    , valD
        (varP formatName)
        (normalB [| dimapJsonFormat $(varE extractorName) $(conE wrapperName) (recJsonFormat $recFormatExp) |])
        []
    , instanceD
        (cxt [])
        [t| FromJSON $(conT wrapperName) |]
        [ funD
            (mkName "parseJSON")
            [ clause [] (normalB [| parseJsonWithFormat' $(varE formatName) |]) [] ]
        ]
    , instanceD
        (cxt [])
        [t| ToJSON $(conT wrapperName) |]
        [ funD
            (mkName "toJSON")
            [ clause [] (normalB [| toJsonWithFormat $(varE formatName) |]) [] ]
        ]
    ]

