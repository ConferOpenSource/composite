#! /usr/bin/env stack
{- stack --resolver lts-9.20 --install-ghc runghc --package myawesomeserver -}
{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import ClassyPrelude
import Data.Aeson (encode)
import qualified Options.Applicative as O
import Turtle (procs)

import Api (swaggerApiDefinition)

data Opts = Opts
  { optsOutputDir      :: Text
  , optsSwaggerJarPath :: FilePath
  }

parseArgs :: IO Opts
parseArgs =
  let parser = Opts
        <$> textField "output-dir" "directory where swagger specs will be written"
        <*> stringField "swagger-codegen-jar-path" "path to swagger codegen"
  in createParser "Generate swagger specs" parser

  where
    createParser d p = O.execParser $ O.info (O.helper <*> p) (O.fullDesc <> O.progDesc d)
    stringField l h = O.strOption (O.long l <> O.metavar (toUpper l) <> O.help h)
    textField l h = pack <$> stringField l h

main :: IO ()
main = do
  Opts {..} <- parseArgs
  let specName = "swagger.json"
      pkgName = "myawesomeserver"
      specPathStr = optsOutputDir <> "/" <> specName
      genPathStr = optsOutputDir <> "/" <> pkgName <> "_gen"

  writeFile (unpack specPathStr) . toStrict . encode $ swaggerApiDefinition
  putStrLn $ "wrote API spec: " <> specPathStr

  procs "java" [ "-jar", pack optsSwaggerJarPath
               , "generate", "-i", specPathStr
               , "-l", "python"
               , "-D", "packageName=" <> pkgName
               , "-o", genPathStr ] mempty
