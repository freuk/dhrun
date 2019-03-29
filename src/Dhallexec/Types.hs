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

module Dhallexec.Types
  ( Verbosity(..)
  , DhallExec(..)
  )
where

import           Protolude
import           Dhall

data Cmd = Cmd {
    name :: Text
  , args :: [Text]
  } deriving (Show, Generic, Interpret)

data DhallExec = DhallExec
  { cmds :: [Cmd]
  } deriving (Show, Generic, Interpret)

data Verbosity = Normal | Verbose
  deriving (Show, Read, Eq, Generic, Interpret)
