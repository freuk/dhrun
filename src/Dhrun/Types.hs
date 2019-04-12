{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
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
  , EnvVar(..)
  , Check(..)
  , FileCheck(..)
  , inputDhallExec
  )
where

import           Protolude
import           Dhall
import           Data.Yaml
import           Data.Aeson

data Verbosity = Normal | Verbose
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, Interpret)

data EnvVar = EnvVar {
    varname :: Text
  , value   :: Text
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON EnvVar where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data Check = Check {
    avoids :: [Text]
  , wants  :: [Text]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)

data DhallExec = DhallExec
  { cmds       :: [Cmd],
    workdir    :: Text,
    verbosity  :: Verbosity,
    pre        :: [Text],
    post       :: [Text]
  } deriving (Show, Eq, Generic, Interpret)

data FileCheck a = FileCheck {
    filename  :: Text
  , filecheck :: a
  } deriving (Eq, Show, Generic, ToJSON, Interpret, Functor)

data Cmd = Cmd {
    name        :: Text
  , args        :: [Text]
  , env         :: [EnvVar]
  , out         :: FileCheck Check
  , err         :: FileCheck Check
  , postchecks  :: [FileCheck Check]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)

inputDhallExec :: (MonadIO m) => Text -> m DhallExec
inputDhallExec fn = liftIO $ try (input ft (fn <> ".dh")) >>= \case
  Right d -> return d
  Left  e -> throwError e
 where
  ft :: Dhall.Type DhallExec
  ft = Dhall.auto
