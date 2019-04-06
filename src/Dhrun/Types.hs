{-# language DeriveAnyClass #-}
{-# language MultiParamTypeClasses #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language NoImplicitPrelude #-}

{-|
Module      : Dhrun.Types
Description : types
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Types
  ( Verbosity(..)
  , DhallExec(..)
  , Cmd(..)
  )
where

import           Protolude
import           Dhall
import           Data.Yaml
import           Data.Aeson

data Verbosity = Normal | Verbose
  deriving (Read, Eq, Generic, FromJSON, ToJSON, Interpret)

data Cmd = Cmd {
    name        :: Text
  , args        :: Maybe [Text]
  , vars        :: Maybe [EnvVar]
  , stdoutcheck :: Maybe Check
  , stderrcheck :: Maybe Check
  , postchecks  :: Maybe [FileCheck]
  } deriving (Eq, Generic, ToJSON, Interpret)
instance FromJSON Cmd where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data EnvVar = EnvVar {
    varname :: Text
  , value   :: Text
  } deriving (Eq, Generic, ToJSON, Interpret)
instance FromJSON EnvVar where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data FileCheck = FileCheck {
    filename  :: Text
  , filecheck :: Check
  } deriving (Eq, Generic, ToJSON, Interpret)
instance FromJSON FileCheck where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data Check = Check {
    avoids   :: Maybe [Text]
  , wants    :: Maybe [Text]
  } deriving (Eq, Generic, ToJSON, Interpret)
instance FromJSON Check where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data DhallExec = DhallExec
  { cmds      :: [Cmd],
    verbosity :: Maybe Verbosity,
    pre       :: Maybe [Text],
    post      :: Maybe [Text]
  } deriving (Eq, Generic, FromJSON, ToJSON, Interpret)
