{-# language DeriveAnyClass #-}
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

module Dhrun.AesonTypes
  ( DhallExec(..)
  , decodeDhallExec
  , encodeDhallExec
  , encodeCmd
  , fromInternal
  , toInternal
  )
where

import qualified Dhrun.Types                   as DT
import           Protolude
import           Dhall
import           Data.Yaml
import           Data.Aeson
import           System.IO.Error

data CheckParse = CheckParse {
    avoids   :: Maybe [Text]
  , wants    :: Maybe [Text]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON CheckParse where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }
instance FromJSON (DT.FileCheck CheckParse) where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

data DhallExec = DhallExec
  { cmds      :: [Cmd],
    workdir   :: Maybe Text,
    verbosity :: Maybe DT.Verbosity,
    pre       :: Maybe [Text],
    post      :: Maybe [Text]
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Cmd = Cmd {
    name        :: Text
  , args        :: Maybe [Text]
  , vars        :: Maybe [DT.EnvVar]
  , passvars    :: Maybe [Text]
  , timeout     :: Maybe Natural
  , out         :: DT.FileCheck CheckParse
  , err         :: DT.FileCheck CheckParse
  , postchecks  :: Maybe [DT.FileCheck CheckParse]
  } deriving (Eq, Show, Generic, ToJSON, Interpret)
instance FromJSON Cmd where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }

fromInternalCheck :: DT.Check -> CheckParse
fromInternalCheck c = CheckParse {..}
 where
  avoids = case DT.avoids c of
    [] -> Nothing
    l  -> Just l
  wants = case DT.wants c of
    [] -> Nothing
    l  -> Just l

toInternalCheck :: CheckParse -> DT.Check
toInternalCheck cp =
  DT.Check {wants = fromMaybe [] (wants cp), avoids = fromMaybe [] (avoids cp)}

toInternalCmd :: Cmd -> DT.Cmd
toInternalCmd c = DT.Cmd
  { name       = name c
  , args       = fromMaybe [] $ args c
  , vars       = fromMaybe [] $ vars c
  , passvars   = fromMaybe [] $ passvars c
  , out        = toInternalCheck <$> out c
  , err        = toInternalCheck <$> err c
  , postchecks = case postchecks c of
    (Just l) -> (toInternalCheck <$>) <$> l
    Nothing  -> []
  , timeout    = timeout c
  }

toInternal :: DhallExec -> DT.DhallExec
toInternal d = DT.DhallExec
  { cmds      = toInternalCmd <$> cmds d
  , verbosity = fromMaybe DT.Normal (verbosity d)
  , pre       = fromMaybe [] (pre d)
  , post      = fromMaybe [] (post d)
  , workdir   = fromMaybe "./" (workdir d)
  }

fromInternalCmd :: DT.Cmd -> Cmd
fromInternalCmd c = Cmd {..}
 where
  name    = DT.name c
  out     = fromInternalCheck <$> DT.out c
  err     = fromInternalCheck <$> DT.err c
  timeout = DT.timeout c
  args    = case DT.args c of
    [] -> Nothing
    l  -> Just l
  vars = case DT.vars c of
    [] -> Nothing
    l  -> Just l
  passvars = case DT.passvars c of
    [] -> Nothing
    l  -> Just l
  postchecks = case DT.postchecks c of
    [] -> Nothing
    l  -> Just ((fromInternalCheck <$>) <$> l)

fromInternal :: DT.DhallExec -> DhallExec
fromInternal d = DhallExec {..}
 where
  workdir = case DT.workdir d of
    "./" -> Nothing
    w    -> Just w
  cmds      = fromInternalCmd <$> DT.cmds d
  verbosity = case DT.verbosity d of
    DT.Normal  -> Nothing
    DT.Verbose -> Just DT.Normal
  pre = case DT.pre d of
    [] -> Nothing
    l  -> Just l
  post = case DT.post d of
    [] -> Nothing
    l  -> Just l

decodeDhallExec :: (MonadIO m) => Text -> m DT.DhallExec
decodeDhallExec fn = liftIO $ try (decodeFileEither (toS fn)) >>= \case
  Left  e          -> throwError e
  Right (Left  pa) -> throwError $ userError $ "parse fail:" <> show pa
  Right (Right a ) -> return $ toInternal a

encodeDhallExec :: DT.DhallExec -> ByteString
encodeDhallExec = Data.Yaml.encode . fromInternal

encodeCmd :: DT.Cmd -> ByteString
encodeCmd = Data.Yaml.encode . fromInternalCmd
