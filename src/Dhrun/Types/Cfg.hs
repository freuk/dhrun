{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Dhrun.Types.Cfg
Description : types
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}
module Dhrun.Types.Cfg
  ( Cfg (..)
  , Verbosity (..)
  , Cmd (..)
  , EnvVar (..)
  , VarName (..)
  , VarValue (..)
  , WorkDir (..)
  , WorkDirBehavior (..)
  , Check (..)
  , FileCheck (..)
  , FileName (..)
  , CommandName (..)
  , Arg (..)
  , Pattern (..)
  , examples
  , Examples (..)
  )
where

import Data.Default
import Dhall
import Protolude
import qualified Prelude (String)

newtype Arg = Arg Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv Arg Text where

  strConv _ (Arg x) = toS x

instance StringConv Arg Prelude.String where

  strConv _ (Arg x) = toS x

newtype CommandName = CommandName Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv CommandName Text where

  strConv _ (CommandName x) = toS x

instance StringConv CommandName FilePath where

  strConv _ (CommandName x) = toS x

newtype Pattern = Pattern Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject, IsString) via Text

instance StringConv Pattern ByteString where

  strConv _ (Pattern x) = toS x

instance StringConv Pattern Text where

  strConv _ (Pattern x) = toS x

newtype FileName = FileName Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv FileName Text where

  strConv _ (FileName x) = toS x

instance StringConv FileName FilePath where

  strConv _ (FileName x) = toS x

newtype VarName = VarName Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv VarName Text where

  strConv _ (VarName x) = toS x

newtype VarValue = VarValue Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv VarValue Text where

  strConv _ (VarValue x) = toS x

newtype Pre = Pre Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv Pre Text where

  strConv _ (Pre x) = toS x

newtype Post = Post Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv Post Text where

  strConv _ (Post x) = toS x

newtype WorkDir = WorkDir Text
  deriving (Eq, Show, Generic)
  deriving (Interpret, Inject) via Text

instance StringConv WorkDir Text where

  strConv _ (WorkDir x) = toS x

instance StringConv WorkDir FilePath where

  strConv _ (WorkDir x) = toS x

data WorkDirBehavior = Keep | Remove
  deriving (Read, Show, Eq, Generic, Interpret, Inject)

data Verbosity = Normal | Verbose
  deriving (Read, Show, Eq, Generic, Interpret, Inject)

data EnvVar
  = EnvVar
      { varname :: VarName
      , value :: VarValue
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data Check
  = Check
      { avoids :: [Pattern]
      , wants :: [Pattern]
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data FileCheck a
  = FileCheck
      { filename :: FileName
      , filecheck :: a
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data Cmd
  = Cmd
      { name :: CommandName
      , exitcode :: Maybe ExitCode
      , args :: [Arg]
      , vars :: [EnvVar]
      , passvars :: [VarName]
      , out :: FileCheck [Text]
      , err :: FileCheck [Text]
      , postchecks :: [FileCheck Check]
      , timeout :: Maybe Int
      , otherwd :: Maybe WorkDir
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data Cfg
  = Cfg
      { cmds :: [Cmd]
      , workdir :: WorkDir
      , cleaning :: WorkDirBehavior
      , verbosity :: Verbosity
      , pre :: [Pre]
      , post :: [Post]
      }
  deriving (Show, Eq, Generic, Interpret, Inject)

deriving instance Interpret ExitCode

deriving instance Inject ExitCode

instance Default Cfg where

  def = Cfg
    { cmds = []
    , workdir = WorkDir "./"
    , cleaning = Keep
    , verbosity = Normal
    , pre = []
    , post = []
    }

instance Interpret Int where

  autoWith _ = fmap fromInteger integer

data Examples
  = Examples
      { successes :: [(Text, Cfg)]
      , failures :: [(Text, Cfg)]
      }

examples :: Examples
examples = Examples
  { successes = [ ( "success-exit1"
                  , defV {cmds = [exitWithCode 1]}
                  )
                , ( "success-exit5"
                  , defV {cmds = [exitWithCode 5]}
                  )
                , ( "success-echo"
                  , defV {cmds = [echo "arbitrary-character-string"]}
                  )
                ]
  , failures = [ ( "failure-pattern-miss"
                 , defV
                   { cmds = [ emptyCmd
                                { postchecks = [ FileCheck
                                                   { filename = (filename $ out emptyCmd)
                                                   , filecheck = Check {avoids = [], wants = ["something that isn't there"]}
                                                   }
                                               ]
                                }
                            ]
                   }
                 )
               , ( "failure-pattern-avoid"
                 , defV
                   { cmds = [ (echo "toavoid")
                                { out = (out emptyCmd)
                                    { filecheck = ["toavoid"]
                                    }
                                }
                            ]
                   }
                 )
               ]
  }
  where
    defV = def {verbosity = Verbose}
    exitWithCode i =
      emptyCmd
        { name = CommandName "bash"
        , args = [Arg "-c", Arg $ "exit " <> show i]
        , exitcode = Just (ExitFailure i)
        }
    echo f =
      emptyCmd
        { name = CommandName "echo"
        , args = [Arg f]
        , postchecks = [ FileCheck
                           { filename = (filename $ out emptyCmd)
                           , filecheck = Check
                             { avoids = []
                             , wants = [Pattern f]
                             }
                           }
                       ]
        }
    emptyCheck fn = FileCheck
      { filename = FileName fn
      , filecheck = []
      }
    emptyCmd = Cmd
      { name = CommandName "echo"
      , args = []
      , exitcode = Just ExitSuccess
      , vars = []
      , err = emptyCheck "err.err"
      , out = emptyCheck "out.out"
      , passvars = [VarName "PATH"]
      , postchecks = []
      , timeout = Nothing
      , otherwd = Nothing
      }
