{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
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
  , DhallExecParse(..)
  , DhallExec(..)
  , Cmd(..)
  , Check(..)
  , FileCheck(..)
  , inputDhallExec
  , decodeDhallExec
  , encodeDhallExec
  , fromInternal
  , toInternal
  )
where

import           Protolude
import           Dhall
import           Data.Yaml
import           Data.Aeson
import           System.IO.Error

data Verbosity = Normal | Verbose
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, Interpret)

data EnvVar = EnvVar {
    varname :: Text
  , value   :: Text
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON EnvVar where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data FileCheck a = FileCheck {
    filename  :: Text
  , filecheck :: a
  } deriving (Eq, Show, Generic, ToJSON, Interpret, Functor)
instance FromJSON (FileCheck CheckParse) where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data Check = Check {
    avoidInfixes :: [Text]
  , wantInfixes  :: [Text]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
data CheckParse = CheckParse {
    avoids   :: Maybe [Text]
  , wants    :: Maybe [Text]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON CheckParse where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data DhallExecParse = DhallExecParse
  { cmds      :: [CmdParse],
    verbosity :: Maybe Verbosity,
    pre       :: Maybe [Text],
    post      :: Maybe [Text]
  } deriving (Eq, Show, Generic, FromJSON, ToJSON, Interpret)

data DhallExec = DhallExec
  { processSpecs   :: [Cmd],
    verbosityLevel :: Verbosity,
    preCmds        :: [Text],
    postCmds       :: [Text]
  } deriving (Show, Eq)

data CmdParse = CmdParse {
    name        :: Text
  , args        :: Maybe [Text]
  , vars        :: Maybe [EnvVar]
  , stdoutcheck :: Maybe CheckParse
  , stderrcheck :: Maybe CheckParse
  , postchecks  :: Maybe [FileCheck CheckParse]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON CmdParse where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data Cmd = Cmd {
    binaryName  :: Text
  , argList     :: [Text]
  , envVars     :: [EnvVar]
  , stdoutCheck :: Maybe Check
  , stderrCheck :: Maybe Check
  , postChecks  :: [FileCheck Check]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)

fromInternalCheck :: Check -> CheckParse
fromInternalCheck Check {..} = CheckParse {..}
 where
  avoids = case avoidInfixes of
    [] -> Nothing
    l  -> Just l
  wants = case wantInfixes of
    [] -> Nothing
    l  -> Just l

toInternalCheck :: CheckParse -> Check
toInternalCheck CheckParse {..} = Check {..}
 where
  wantInfixes = case wants of
    (Just l) -> l
    Nothing  -> []
  avoidInfixes = case avoids of
    (Just l) -> l
    Nothing  -> []

toInternalCmd :: CmdParse -> Cmd
toInternalCmd CmdParse {..} = Cmd {..}
 where
  binaryName = name
  argList    = case args of
    (Just l) -> l
    Nothing  -> []
  envVars = case vars of
    (Just l) -> l
    Nothing  -> []
  stdoutCheck = toInternalCheck <$> stdoutcheck
  stderrCheck = toInternalCheck <$> stderrcheck
  postChecks  = case postchecks of
    (Just l) -> (toInternalCheck <$>) <$> l
    Nothing  -> []

toInternal :: DhallExecParse -> DhallExec
toInternal DhallExecParse {..} = DhallExec {..}
 where
  processSpecs   = toInternalCmd <$> cmds
  verbosityLevel = case verbosity of
    (Just Verbose) -> Normal
    (Just Normal ) -> Normal
    Nothing        -> Normal
  preCmds = case pre of
    (Just l) -> l
    Nothing  -> []
  postCmds = case post of
    (Just l) -> l
    Nothing  -> []

fromInternalCmd :: Cmd -> CmdParse
fromInternalCmd Cmd {..} = CmdParse {..}
 where
  name        = binaryName
  stdoutcheck = fromInternalCheck <$> stdoutCheck
  stderrcheck = fromInternalCheck <$> stderrCheck
  args        = case argList of
    [] -> Nothing
    l  -> Just l
  vars = case envVars of
    [] -> Nothing
    l  -> Just l
  postchecks = case postChecks of
    [] -> Nothing
    l  -> Just ((fromInternalCheck <$>) <$> l)

fromInternal :: DhallExec -> DhallExecParse
fromInternal DhallExec {..} = DhallExecParse {..}
 where
  cmds      = fromInternalCmd <$> processSpecs
  verbosity = case verbosityLevel of
    Normal  -> Nothing
    Verbose -> Just Normal
  pre = case preCmds of
    [] -> Nothing
    l  -> Just l
  post = case postCmds of
    [] -> Nothing
    l  -> Just l

inputDhallExec :: (MonadIO m) => Text -> m DhallExec
inputDhallExec fn = liftIO $ try (input ft (fn <> ".dh")) >>= \case
  Right d -> return $ toInternal d
  Left  e -> throwError e
 where
  ft :: Dhall.Type DhallExecParse
  ft = Dhall.auto

decodeDhallExec :: (MonadIO m) => Text -> m DhallExec
decodeDhallExec fn =
  liftIO $ try (decodeFileEither (toS $ fn <> ".yml")) >>= \case
    Left  e          -> throwError e
    Right (Left  pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a ) -> return $ toInternal a

encodeDhallExec :: DhallExec -> ByteString
encodeDhallExec = Data.Yaml.encode . fromInternal
