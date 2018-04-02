module Composite.Aeson.TH
  ( makeFieldJsonWrapper, makeFieldJsonWrapperExplicit
  , makeRecordJsonWrapper, makeRecordJsonWrapperExplicit
  ) where

import Composite.Aeson.Base (JsonFormat, dimapJsonFormat, parseJsonWithFormat', toJsonWithFormat)
import Composite.Aeson.CoRecord (defaultJsonFormatField, fieldJsonFormat)
import Composite.Aeson.Formats.Default (DefaultJsonFormat, defaultJsonFormat)
import Composite.Aeson.Formats.Generic (SumStyle)
import Composite.Aeson.Record (defaultJsonFormatRecord, recordJsonFormat)
import Composite.CoRecord (Field)
import Composite.Record (Record)
import Control.Lens (_head, over)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Char (toLower)
import Data.Monoid ((<>))
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
import Language.Haskell.TH.Syntax (lift)

-- |TH splice which makes it more convenient to define 'DefaultJsonFormat', 'ToJSON', and 'FromJSON' instances for 'Field' types.
--
-- For example:
--
-- @
--   type MyField = '[FFoo, FBar]
--   makeFieldJsonWrapper "MyFieldJson" ''MyField SumStyleFieldName
-- @
--
-- is equivalent to:
--
-- @
--   newtype MyFieldJson = MyFieldJson { unMyFieldJson :: Field MyField }
--   myFieldJsonFormat :: 'Composite.Aeson.CoRecord.JsonFormatField' Void MyFieldJson
--   myFieldJsonFormat =
--     'dimapJsonFormat' unMyFieldJson MyFieldJson $
--       'fieldJsonFormat' SumStyleFieldName defaultJsonFormatField
--   instance FromJSON MyFieldJson where
--     parseJSON = 'parseJsonWithFormat'' myFieldJsonFormat
--   instance ToJSON MyFieldJson where
--     toJSON = 'toJsonWithFormat' myFieldJsonFormat
-- @
--
-- This function uses 'defaultJsonFormatField' to derive the formatting for the field. If you want to customize that formatting, use
-- 'makeFieldJsonWrapperExplicit' instead.
makeFieldJsonWrapper :: String -> Name -> SumStyle -> Q [Dec]
makeFieldJsonWrapper wrapperName tyName sumStyle =
  makeFieldJsonWrapperExplicit wrapperName tyName sumStyle [| defaultJsonFormatField |]

-- |TH splice which makes it more convenient to define 'DefaultJsonFormat', 'ToJSON', and 'FromJSON' instances for 'Field' types.
--
-- For example:
--
-- @
--   type MyField = '[FFoo, FBar]
--   makeRecJsonWrapperExplicit "MyFieldJson" ''MyField [| set (rlens fFoo_) specialFormat defaultJsonFormatRecord |]
-- @
--
-- is equivalent to:
--
-- @
--   newtype MyFieldJson = MyFieldJson { unMyFieldJson :: Record MyField }
--   myRecordJsonFormat :: 'Composite.Aeson.Record.JsonFormatRecord' Void MyFieldJson
--   myRecordJsonFormat =
--     'dimapJsonFormat' unMyFieldJson MyFieldJson $
--       'recordJsonFormat' (set (rlens fFoo_) specialFormat defaultJsonFormatRecord)
--   instance FromJSON MyFieldJson where
--     parseJSON = 'parseJsonWithFormat'' myRecordJsonFormat
--   instance ToJSON MyFieldJson where
--     toJSON = 'toJsonWithFormat' myRecordJsonFormat
-- @
makeFieldJsonWrapperExplicit :: String -> Name -> SumStyle -> Q Exp -> Q [Dec]
makeFieldJsonWrapperExplicit wrapperNameStr fieldsTyName sumStyle fieldFormatExp = do
  let wrapperName = mkName wrapperNameStr
      extractorName = mkName $ "un" <> wrapperNameStr
      fieldTy = [t| Field $(conT fieldsTyName) |]
  formatName <- newName $ over _head toLower wrapperNameStr <> "Format"
  sequence
    [ newtypeD
        (cxt [])
        wrapperName
        [] -- TyVarBndrs
        Nothing -- kind
        (recC wrapperName [varBangType extractorName (bangType (bang noSourceUnpackedness noSourceStrictness) fieldTy)])
        [] -- deriving context
    , sigD
        formatName
        [t| forall e. JsonFormat e $(conT wrapperName) |]
    , valD
        (varP formatName)
        (normalB [| dimapJsonFormat $(varE extractorName) $(conE wrapperName) (fieldJsonFormat $(lift sumStyle) $fieldFormatExp) |])
        []
    , instanceD
        (cxt [])
        [t| DefaultJsonFormat $(conT wrapperName) |]
        [ funD
            'defaultJsonFormat
            [ clause [] (normalB $ varE formatName) [] ]
        ]
    , instanceD
        (cxt [])
        [t| FromJSON $(conT wrapperName) |]
        [ funD
            'parseJSON
            [ clause [] (normalB [| parseJsonWithFormat' $(varE formatName) |]) [] ]
        ]
    , instanceD
        (cxt [])
        [t| ToJSON $(conT wrapperName) |]
        [ funD
            'toJSON
            [ clause [] (normalB [| toJsonWithFormat $(varE formatName) |]) [] ]
        ]
    ]

-- |TH splice which makes it more convenient to define 'DefaultJsonFormat', 'ToJSON', and 'FromJSON' instances for 'Record' types.
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
--   myRecordJsonFormat :: 'Composite.Aeson.Record.JsonFormatRecord' Void MyRecordJson
--   myRecordJsonFormat =
--     'dimapJsonFormat' unMyRecordJson MyRecordJson $
--       'recordJsonFormat' defaultJsonFormatRecord
--   instance FromJSON MyRecordJson where
--     parseJSON = 'parseJsonWithFormat'' myRecordJsonFormat
--   instance ToJSON MyRecordJson where
--     toJSON = 'toJsonWithFormat' myRecordJsonFormat
-- @
--
-- This function uses 'defaultJsonFormatRecord' to derive the formatting for the record. If you want to customize that formatting, use
-- 'makeRecJsonWrapperExplicit' instead.
makeRecordJsonWrapper :: String -> Name -> Q [Dec]
makeRecordJsonWrapper wrapperName tyName =
  makeRecordJsonWrapperExplicit wrapperName tyName [| defaultJsonFormatRecord |]

-- |TH splice which makes it more convenient to define 'DefaultJsonFormat', 'ToJSON', and 'FromJSON' instances for 'Record' types.
--
-- For example:
--
-- @
--   type MyRecord = '[FFoo, FBar]
--   makeRecordJsonWrapperExplicit "MyRecordJson" ''MyRecord [| set (rlens fFoo_) specialFormat defaultJsonFormatRecord |]
-- @
--
-- is equivalent to:
--
-- @
--   newtype MyRecordJson = MyRecordJson { unMyRecordJson :: Record MyRecord }
--   myRecordJsonFormat :: 'Composite.Aeson.Record.JsonFormatRecord' Void MyRecordJson
--   myRecordJsonFormat =
--     'dimapJsonFormat' unMyRecordJson MyRecordJson $
--       'recordJsonFormat' (set (rlens fFoo_) specialFormat defaultJsonFormatRecord)
--   instance DefaultJsonFormat MyRecordJson where
--     defaultJsonFormat = myRecordJsonFormat
--   instance FromJSON MyRecordJson where
--     parseJSON = 'parseJsonWithFormat'' myRecordJsonFormat
--   instance ToJSON MyRecordJson where
--     toJSON = 'toJsonWithFormat' myRecordJsonFormat
-- @
makeRecordJsonWrapperExplicit :: String -> Name -> Q Exp -> Q [Dec]
makeRecordJsonWrapperExplicit wrapperNameStr fieldsTyName recFormatExp = do
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
        [] -- deriving context
    , sigD
        formatName
        [t| forall e. JsonFormat e $(conT wrapperName) |]
    , valD
        (varP formatName)
        (normalB [| dimapJsonFormat $(varE extractorName) $(conE wrapperName) (recordJsonFormat $recFormatExp) |])
        []
    , instanceD
        (cxt [])
        [t| DefaultJsonFormat $(conT wrapperName) |]
        [ funD
            'defaultJsonFormat
            [ clause [] (normalB $ varE formatName) [] ]
        ]
    , instanceD
        (cxt [])
        [t| FromJSON $(conT wrapperName) |]
        [ funD
            'parseJSON
            [ clause [] (normalB [| parseJsonWithFormat' $(varE formatName) |]) [] ]
        ]
    , instanceD
        (cxt [])
        [t| ToJSON $(conT wrapperName) |]
        [ funD
            'toJSON
            [ clause [] (normalB [| toJsonWithFormat $(varE formatName) |]) [] ]
        ]
    ]

