{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : dhall-exec.hs
Description : dhall-exec main app file.
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}
module Main
  ( main
  )
where

import qualified Data.ByteString as B
  ( getContents
  )
import Dhall
import Dhrun.Run as DR
import Dhrun.Types.Cfg as DI
import GHC.IO.Encoding
import Options.Applicative as OA
import Protolude
import System.Directory
import System.FilePath.Posix
import qualified System.IO as SIO
import Text.Editor
import qualified Prelude
  ( print
  )

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding SIO.utf8
  join . customExecParser (prefs showHelpOnError) $
    info
      (helper <*> opts)
      ( fullDesc <> header "dhrun" <>
        progDesc
          ( "dhall-configured concurrent process execution" <>
            " with streaming assertion monitoring"
          )
      )

data MainCfg
  = MainCfg
      { inputfile :: Maybe Text
      , stdinType :: SourceType
      , verbosity :: Verbosity
      , edit :: Bool
      }

commonParser :: Parser MainCfg
commonParser =
  MainCfg <$>
    optional
      ( strArgument
        ( metavar "INPUT" <>
          help
            ( "Input configuration with .yml/.yaml/.dh/.dhall extension." <>
              " Leave void for stdin (dhall) input."
            )
        )
      ) <*>
    flag
      Dhall
      Yaml
      ( long "yaml" <> short 'y' <>
        help
          "Assume stdin to be yaml instead of dhall."
      ) <*>
    flag Normal
      Verbose
      (long "verbose" <> short 'v' <> help "Enable verbose mode.") <*>
    flag
      False
      True
      (long "edit" <> short 'e' <> help "Edit yaml in $EDITOR before run.")

opts :: Parser (IO ())
opts =
  hsubparser $
    command
      "run"
      (info (run <$> commonParser) $ progDesc "Run a dhrun specification.") <>
    command
      "print"
      ( info (printY <$> commonParser) $
        progDesc "Print a dhrun specification."
      ) <>
    help "Type of operation to run."

data SourceType = Dhall | Yaml
  deriving (Eq)

data FinallySource = NoExt | FinallyFile SourceType Text | FinallyStdin SourceType

ext :: SourceType -> Maybe Text -> FinallySource
ext _ (Just fn)
  | xt `elem` [".dh", ".dhall"] = FinallyFile Dhall fn
  | xt `elem` [".yml", ".yaml"] = FinallyFile Yaml fn
  | otherwise = NoExt
  where
    xt = takeExtension $ toS fn
ext st Nothing = FinallyStdin st

load :: MainCfg -> IO Cfg
load MainCfg {..} =
  (if edit then editing else return) =<<
    overrideV <$> case ext stdinType inputfile of
    (FinallyFile Dhall filename) ->
      (if v then detailed else identity) $
        inputCfg =<<
        toS <$>
        makeAbsolute (toS filename)
    (FinallyFile Yaml filename) ->
      decodeCfgFile =<< toS <$> makeAbsolute (toS filename)
    (FinallyStdin Yaml) ->
      B.getContents <&> decodeCfg >>= \case
        Left e -> Prelude.print e >> die "yaml parsing exception."
        Right cfg -> return cfg
    (FinallyStdin Dhall) -> B.getContents >>= inputCfg . toS
    NoExt ->
      die
        ( "couldn't figure out extension for input file. " <>
          "Please use something in {.yml,.yaml,.dh,.dhall} ."
        )
  where
    v = verbosity == Verbose
    overrideV x =
      x
        { DI.verbosity = if (DI.verbosity x == Verbose) || v
          then Verbose
          else Normal
        }

editing :: Cfg -> IO Cfg
editing c =
  runUserEditorDWIM yt (encodeCfg c) <&> decodeCfg >>= \case
    Left e -> Prelude.print e >> die "yaml parsing exception."
    Right cfg -> return cfg
  where
    yt = mkTemplate "yaml"

run :: MainCfg -> IO ()
run c = load c >>= DR.runDhrun

printY :: MainCfg -> IO ()
printY c = load c >>= putText . toS . encodeCfg
