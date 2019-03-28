{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language NoImplicitPrelude #-}

{-|
Module      : Dhallexec.Types
Description : types for running argo stacks
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Argo.Types
  ( StdOutLog(..)
  , StdErrLog(..)
  , TestText(..)
  , ProcessBehavior(..)
  , TextBehavior(..)
  , TextBehaviorStdout(..)
  , TextBehaviorStderr(..)
  , WorkingDirectory(..)
  , Verbosity(..)
  , AppName(..)
  , AppArg(..)
  , EnvVar(..)
  , ContainerName(..)
  , ShareDir(..)
  , ManifestName(..)
  , StackArgs(..)
  , PreludeCommand(..)
  , HwThreadCount(..)
  , PowerCap(..)
  , toOption
  )
where

import           Data.Default
import           Data.Text                     as T
                                         hiding ( empty )
import           Protolude
import           Data.Yaml
import           Dhall

data StackArgs = StackArgs
  { verbosity              :: Verbosity
  , app                    :: AppName
  } deriving (Show, Generic, ToJSON, FromJSON, Interpret)

{-data OutputFiles = OutputFiles Text Text-}
data Verbosity = Normal | Verbose
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, Interpret)
newtype AppName = AppName Text
  deriving stock (Show, Read, Generic)
  deriving newtype (IsString)
  deriving anyclass (FromJSON, ToJSON, Interpret)
