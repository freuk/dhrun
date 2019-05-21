{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language DeriveGeneric #-}

{-|
Module      : Dhrun.Internal
Description : types
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Types.Cfg
  ( Cfg(..)
  , Verbosity(..)
  , Cmd(..)
  , EnvVar(..)
  , VarName(..)
  , VarValue(..)
  , WorkDir(..)
  , WorkDirBehavior(..)
  , Check(..)
  , FileCheck(..)
  , FileName(..)
  , CommandName(..)
  , Arg(..)
  , Pattern(..)
  , inputCfg
  , decodeCfgFile
  , decodeCfg
  , encodeCfg
  , encodeCmd
  )
where

import           Dhall
import qualified Dhrun.Types.Dhall             as DT
import qualified Dhrun.Types.Yaml              as DAT
import           Data.Yaml.Internal
import           Protolude
import qualified Prelude                        ( String )

newtype Arg = Arg Text deriving (Eq, Show, Generic)
instance StringConv Arg Text where
  strConv _ (Arg x) = toS x
instance StringConv Arg Prelude.String where
  strConv _ (Arg x) = toS x

newtype CommandName = CommandName Text deriving (Eq, Show, Generic)
instance StringConv CommandName Text where
  strConv _ (CommandName x) = toS x
instance StringConv CommandName FilePath where
  strConv _ (CommandName x) = toS x

newtype Pattern = Pattern Text deriving (Eq, Show, Generic)
instance StringConv Pattern ByteString where
  strConv _ (Pattern x) = toS x
instance StringConv Pattern Text where
  strConv _ (Pattern x) = toS x

newtype FileName = FileName Text deriving (Eq, Show, Generic)
instance StringConv FileName Text where
  strConv _ (FileName x) = toS x
instance StringConv FileName FilePath where
  strConv _ (FileName x) = toS x

newtype VarName = VarName Text deriving (Eq, Show, Generic)
instance StringConv VarName Text where
  strConv _ (VarName x) = toS x

newtype VarValue = VarValue Text deriving (Eq, Show, Generic)
instance StringConv VarValue Text where
  strConv _ (VarValue x) = toS x

newtype Pre = Pre Text deriving (Eq, Show, Generic)
instance StringConv Pre Text where
  strConv _ (Pre x) = toS x

newtype Post = Post Text deriving (Eq, Show, Generic)
instance StringConv Post Text where
  strConv _ (Post x) = toS x

newtype WorkDir = WorkDir Text deriving (Eq, Show, Generic)
instance StringConv WorkDir Text where
  strConv _ (WorkDir x) = toS x
instance StringConv WorkDir FilePath where
  strConv _ (WorkDir x) = toS x

data WorkDirBehavior = Keep | Remove
  deriving (Read, Show, Eq)

data Verbosity = Normal | Verbose
  deriving (Read, Show, Eq)

data EnvVar = EnvVar {
    varname :: VarName
  , value   :: VarValue
  } deriving (Eq, Show, Generic)


data Check = Check {
    avoids :: [Pattern]
  , wants  :: [Pattern]
  } deriving (Eq, Show, Generic)

data FileCheck a = FileCheck {
    filename  :: FileName
  , filecheck :: a
  } deriving (Eq, Show, Generic)

data Cmd = Cmd {
    name        :: CommandName
  , exitcode    :: Maybe ExitCode
  , args        :: [Arg]
  , vars        :: [EnvVar]
  , passvars    :: [VarName]
  , out         :: FileCheck Check
  , err         :: FileCheck Check
  , postchecks  :: [FileCheck Check]
  , timeout     :: Maybe Int
  , otherwd     :: Maybe WorkDir
  } deriving (Eq, Show, Generic)

data Cfg = Cfg
  { cmds      :: [Cmd],
    workdir   :: WorkDir,
    cleaning  :: WorkDirBehavior,
    verbosity :: Verbosity,
    pre       :: [Pre],
    post      :: [Post]
  } deriving (Show, Eq)

toInternal :: DT.Cfg -> Cfg
toInternal DT.Cfg {..} = Cfg
  { cmds      = toInternalCmd <$> cmds
  , workdir   = WorkDir workdir
  , cleaning  = if cleaning then Remove else Keep
  , pre       = Pre <$> pre
  , post      = Post <$> post
  , verbosity = if verbose then Verbose else Normal
  }

fromInternal :: Cfg -> DT.Cfg
fromInternal Cfg {..} = DT.Cfg
  { cmds     = fromInternalCmd <$> cmds
  , workdir  = toS workdir
  , cleaning = cleaning == Remove
  , pre      = toS <$> pre
  , post     = toS <$> post
  , verbose  = verbosity == Verbose
  }

toInternalCmd :: DT.Cmd -> Cmd
toInternalCmd DT.Cmd {..} = Cmd
  { name       = CommandName name
  , exitcode   = case exitcode of
                   Just 0 -> Just ExitSuccess
                   Just x -> Just $ ExitFailure (fromInteger x)
                   Nothing -> Nothing
  , args       = Arg <$> args
  , vars       = vars <&> \DT.EnvVar {..} ->
    EnvVar {varname = VarName varname, value = VarValue value}
  , passvars   = VarName <$> passvars
  , out        = toInternalFileCheck out
  , err        = toInternalFileCheck err
  , postchecks = toInternalFileCheck <$> postchecks
  , timeout    = fromInteger . toInteger <$> timeout
  , otherwd      = WorkDir <$> otherwd
  }

toInternalFileCheck :: DT.FileCheck DT.Check -> FileCheck Check
toInternalFileCheck DT.FileCheck {..} = FileCheck
  { filename  = FileName filename
  , filecheck = toInternalCheck filecheck
  }

toInternalCheck :: DT.Check -> Check
toInternalCheck DT.Check {..} =
  Check {avoids = Pattern <$> avoids, wants = Pattern <$> wants}

fromInternalCmd :: Cmd -> DT.Cmd
fromInternalCmd Cmd {..} = DT.Cmd
  { out        = fromInternalFileCheck out
  , err        = fromInternalFileCheck err
  , postchecks = fromInternalFileCheck <$> postchecks
  , timeout    = fromInteger . toInteger <$> timeout
  , name       = toS name
  , exitcode   = case exitcode of
                   Just ExitSuccess -> Just 1
                   Just (ExitFailure n) -> Just $ toInteger n
                   Nothing -> Nothing
  , args       = toS <$> args
  , vars       = vars <&> \case
    EnvVar {..} -> DT.EnvVar {varname = toS varname, value = toS value}
  , passvars   = toS <$> passvars
  , otherwd      = toS <$> otherwd
  }

fromInternalFileCheck :: FileCheck Check -> DT.FileCheck DT.Check
fromInternalFileCheck FileCheck {..} = DT.FileCheck
  { filename  = toS filename
  , filecheck = fromInternalCheck filecheck
  }

fromInternalCheck :: Check -> DT.Check
fromInternalCheck Check {..} =
  DT.Check {avoids = toS <$> avoids, wants = toS <$> wants}

decodeCfgFile :: (MonadIO m) => Text -> m Cfg
decodeCfgFile fn = toInternal <$> DAT.decodeCfgFile fn

decodeCfg :: ByteString -> Either Data.Yaml.Internal.ParseException Cfg
decodeCfg t = toInternal <$> DAT.decodeCfg t

encodeCfg :: Cfg -> ByteString
encodeCfg = DAT.encodeCfg . fromInternal

encodeCmd :: Cmd -> ByteString
encodeCmd = DAT.encodeCmd . fromInternalCmd

inputCfg :: (MonadIO m) => Text -> m Cfg
inputCfg fn = toInternal <$> DT.inputCfg fn
