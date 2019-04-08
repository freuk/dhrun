{-# language DeriveAnyClass #-}
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

data Cmd = Cmd {
    name        :: Text
  , args        :: Maybe [Text]
  , vars        :: Maybe [EnvVar]
  , stdoutcheck :: Maybe Check
  , stderrcheck :: Maybe Check
  , postchecks  :: Maybe [FileCheck]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON Cmd where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data EnvVar = EnvVar {
    varname :: Text
  , value   :: Text
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON EnvVar where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data FileCheck = FileCheck {
    filename  :: Text
  , filecheck :: Check
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON FileCheck where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data Check = Check {
    avoids   :: Maybe [Text]
  , wants    :: Maybe [Text]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON Check where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data DhallExecParse = DhallExecParse
  { cmds      :: [Cmd],
    verbosity :: Maybe Verbosity,
    pre       :: Maybe [Text],
    post      :: Maybe [Text]
  } deriving (Eq, Show, Generic, FromJSON, ToJSON, Interpret)

data DhallExec = DhallExec
  { processSpecs   :: [Cmd],
    verbosityLevel :: Verbosity,
    preCmd         :: [Text],
    postCmd        :: [Text]
  } deriving (Show, Eq)

toInternal :: DhallExecParse -> DhallExec
toInternal DhallExecParse {..} = DhallExec {..}
 where
  processSpecs   = cmds
  verbosityLevel = case verbosity of
    (Just Verbose) -> Normal
    (Just Normal ) -> Normal
    Nothing        -> Normal
  preCmd = case pre of
    (Just l) -> l
    Nothing  -> []
  postCmd = case post of
    (Just l) -> l
    Nothing  -> []

fromInternal :: DhallExec -> DhallExecParse
fromInternal DhallExec {..} = DhallExecParse {..}
 where
  cmds      = processSpecs
  verbosity = case verbosityLevel of
    Normal  -> Nothing
    Verbose -> Just Normal
  pre = case preCmd of
    [] -> Nothing
    l  -> Just l
  post = case postCmd of
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

encodeDhallExec :: DhallExec ->  ByteString
encodeDhallExec =  Data.Yaml.encode . fromInternal
