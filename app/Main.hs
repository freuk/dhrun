{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

import           Protolude

import qualified Prelude
                   ( print )
import           Dhrun.Types.Cfg                                   as DI
import           Dhrun.Run                                         as DR
import           Options.Applicative                               as OA
import           Dhall
import           System.FilePath.Posix
import           System.Directory
import           GHC.IO.Encoding
import qualified System.IO                                         as SIO
import qualified Data.ByteString                                   as B
                   ( getContents )
import           Text.Editor

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding SIO.utf8
  join . customExecParser (prefs showHelpOnError) $ info
    (helper <*> opts)
    (fullDesc <> header "dhrun" <> progDesc
      (  "dhall-configured concurrent process execution"
      <> " with streaming assertion monitoring"
      )
    )

data MainCfg = MainCfg
  { inputfile    :: Text
  , yamlStdInput :: SourceType
  , verbosity    :: Verbosity
  , edit :: Bool
}

commonParser :: Parser MainCfg
commonParser =
  MainCfg
    <$> strArgument
          (  metavar "INPUT"
          <> help "Input dhall configuration. Use \"-\" for stdin."
          )
    <*> flag
          Dhall
          Yaml
          (long "yaml" <> short 'y' <> help
            "Read yaml from stdin instead of dhall when using \"-\"."
          )
    <*> flag Normal
             Verbose
             (long "verbose" <> short 'v' <> help "Enable verbose mode.")
    <*> flag False
             True
             (long "edit" <> short 'e' <> help "Edit yaml before run.")

opts :: Parser (IO ())
opts =
  hsubparser
    $  command
         "run"
         (info (run <$> commonParser) $ progDesc "Run a dhrun specification.")
    <> command
         "print"
         ( info (printY <$> commonParser)
         $ progDesc "Print a dhrun specification."
         )
    <> help "Type of operation to run."


data SourceType = Dhall | Yaml deriving (Eq)
data Source = Known SourceType | Stdin
ext :: Text -> Maybe Source
ext "-" = Just Stdin
ext fn | xt `elem` [".dh", ".dhall"] = Just (Known Dhall)
       | xt `elem` [".yml", ".yaml"] = Just (Known Yaml)
       | otherwise                   = Nothing
  where xt = takeExtension $ toS fn

load :: MainCfg -> IO Cfg
load MainCfg {..} =
  (if edit then editing else return)
    =<< overrideV
    <$> case ext (toS inputfile) of
          (Just (Known Dhall)) ->
            (if v then detailed else identity)
              $   inputCfg
              =<< toS
              <$> makeAbsolute (toS inputfile)
          (Just (Known Yaml)) ->
            decodeCfgFile =<< toS <$> makeAbsolute (toS inputfile)
          (Just Stdin) -> case yamlStdInput of
            Yaml -> B.getContents <&> decodeCfg >>= \case
              Left  e   -> Prelude.print e >> die "yaml parsing exception."
              Right cfg -> return cfg
            Dhall -> B.getContents >>= inputCfg . toS
          Nothing ->
            die $ "couldn't figure out extension for file " <> inputfile
 where
  v = verbosity == Verbose
  overrideV x = x
    { DI.verbosity = if (DI.verbosity x == Verbose) || v
                       then Verbose
                       else Normal
    }

editing :: Cfg -> IO Cfg
editing c = runUserEditorDWIM yt (encodeCfg c) <&> decodeCfg >>= \case
  Left  e   -> Prelude.print e >> die "yaml parsing exception."
  Right cfg -> return cfg
  where yt = mkTemplate "yaml"

run :: MainCfg -> IO ()
run c = load c >>= DR.runDhrun

printY :: MainCfg -> IO ()
printY c = load c >>= putText . toS . encodeCfg
