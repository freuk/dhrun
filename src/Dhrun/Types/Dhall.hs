{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}

{-|
Module      : Dhrun.Types
Description : types
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Types.Dhall
  ( Cfg(..)
  , Cmd(..)
  , EnvVar(..)
  , Check(..)
  , FileCheck(..)
  , inputCfg
  )
where

import           Protolude
import           Dhall
import           Data.Yaml
import           Data.Aeson

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

data Cfg = Cfg
  { cmds      :: [Cmd],
    workdir   :: Text,
    cleaning  :: Bool,
    verbose   :: Bool,
    pre       :: [Text],
    post      :: [Text]
  } deriving (Show, Eq, Generic, Interpret)

data FileCheck a = FileCheck {
    filename  :: Text
  , filecheck :: a
  } deriving (Eq, Show, Generic, ToJSON, Interpret, Functor)

data Cmd = Cmd {
    name        :: Text
  , exitcode    :: Maybe Integer
  , args        :: [Text]
  , vars        :: [EnvVar]
  , otherwd     :: Maybe Text
  , passvars    :: [Text]
  , out         :: FileCheck Check
  , err         :: FileCheck Check
  , postchecks  :: [FileCheck Check]
  , timeout     :: Maybe Natural
  } deriving (Eq, Show, Generic, ToJSON, Interpret)

inputCfg :: (MonadIO m) => Text -> m Cfg
inputCfg fn = liftIO $ try (input ft fn) >>= \case
  Right d -> return d
  Left  e -> throwError e
 where
  ft :: Dhall.Type Cfg
  ft = Dhall.auto
