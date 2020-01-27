{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Dhrun.AesonTypes
Description : types
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}
module Dhrun.Types.Yaml
  ( decodeCfgFile
  , decodeCfg
  , encodeCfg
  , encodeCmd
  )
where

import Data.Aeson
import Data.Yaml
import Dhall
import qualified Dhrun.Types.Dhall as DT
import Protolude
import System.IO.Error

data CheckParse
  = CheckParse
      { avoids :: Maybe [Text]
      , wants :: Maybe [Text]
      }
  deriving (Eq, Show, Generic, ToJSON, Interpret)

instance FromJSON CheckParse where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON (DT.FileCheck CheckParse) where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Cfg
  = Cfg
      { cmds :: [Cmd]
      , workdir :: Maybe Text
      , verbose :: Maybe Bool
      , cleaning :: Maybe Bool
      , pre :: Maybe [Text]
      , post :: Maybe [Text]
      }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Cmd
  = Cmd
      { name :: Text
      , exitcode :: Maybe Integer
      , args :: Maybe [Text]
      , vars :: Maybe [DT.EnvVar]
      , passvars :: Maybe [Text]
      , timeout :: Maybe Natural
      , out :: DT.FileCheck CheckParse
      , err :: DT.FileCheck CheckParse
      , postchecks :: Maybe [DT.FileCheck CheckParse]
      , otherwd :: Maybe Text
      }
  deriving (Eq, Show, Generic, ToJSON, Interpret)

instance FromJSON Cmd where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

fromInternalCheck :: DT.Check -> CheckParse
fromInternalCheck c = CheckParse {..}
  where
    avoids = case DT.avoids c of
      [] -> Nothing
      l -> Just l
    wants = case DT.wants c of
      [] -> Nothing
      l -> Just l

toInternalCheck :: CheckParse -> DT.Check
toInternalCheck cp =
  DT.Check {wants = fromMaybe [] (wants cp), avoids = fromMaybe [] (avoids cp)}

toInternalCmd :: Cmd -> DT.Cmd
toInternalCmd c = DT.Cmd
  { name = name c
  , exitcode = exitcode c
  , args = fromMaybe [] $ args c
  , vars = fromMaybe [] $ vars c
  , passvars = fromMaybe [] $ passvars c
  , out = toInternalCheck <$> out c
  , err = toInternalCheck <$> err c
  , postchecks = case postchecks c of
    (Just l) -> (toInternalCheck <$>) <$> l
    Nothing -> []
  , timeout = timeout c
  , otherwd = otherwd c
  }

toInternal :: Cfg -> DT.Cfg
toInternal d = DT.Cfg
  { cmds = toInternalCmd <$> cmds d
  , verbose = fromMaybe False (verbose d)
  , cleaning = fromMaybe False (cleaning d)
  , pre = fromMaybe [] (pre d)
  , post = fromMaybe [] (post d)
  , workdir = fromMaybe "./" (workdir d)
  }

fromInternalCmd :: DT.Cmd -> Cmd
fromInternalCmd c = Cmd {..}
  where
    name = DT.name c
    exitcode = DT.exitcode c
    out = fromInternalCheck <$> DT.out c
    err = fromInternalCheck <$> DT.err c
    timeout = DT.timeout c
    otherwd = DT.otherwd c
    args = case DT.args c of
      [] -> Nothing
      l -> Just l
    vars = case DT.vars c of
      [] -> Nothing
      l -> Just l
    passvars = case DT.passvars c of
      [] -> Nothing
      l -> Just l
    postchecks = case DT.postchecks c of
      [] -> Nothing
      l -> Just ((fromInternalCheck <$>) <$> l)

fromInternal :: DT.Cfg -> Cfg
fromInternal d = Cfg {..}
  where
    workdir = case DT.workdir d of
      "./" -> Nothing
      w -> Just w
    cmds = fromInternalCmd <$> DT.cmds d
    verbose = if DT.verbose d then Just True else Nothing
    cleaning = if DT.cleaning d then Just True else Nothing
    pre = case DT.pre d of
      [] -> Nothing
      l -> Just l
    post = case DT.post d of
      [] -> Nothing
      l -> Just l

decodeCfgFile :: (MonadIO m) => Text -> m DT.Cfg
decodeCfgFile fn =
  liftIO $ try (decodeFileEither (toS fn)) >>= \case
    Left e -> throwError e
    Right (Left pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a) -> return $ toInternal a

decodeCfg :: ByteString -> Either ParseException DT.Cfg
decodeCfg fn = toInternal <$> decodeEither' fn

encodeCfg :: DT.Cfg -> ByteString
encodeCfg = Data.Yaml.encode . fromInternal

encodeCmd :: DT.Cmd -> ByteString
encodeCmd = Data.Yaml.encode . fromInternalCmd
