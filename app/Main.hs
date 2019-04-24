{-# LANGUAGE OverloadedStrings #-}
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

import           Dhrun.Internal                as DI
import           Dhrun.Run                     as DR
import           Options.Applicative           as OA
import           Dhall
import           System.FilePath.Posix
import           System.Directory
import           GHC.IO.Encoding
import           System.IO

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  join . customExecParser (prefs showHelpOnError) $ info
    (helper <*> opts)
    (fullDesc <> header "Dhall-based threaded executor" <> progDesc
      ("This small program allows to configure and execute asynchronous processes."
      <> "It is meant to be used for CI jobs."
      )
    )

data Common = Common
  { inputfile :: Text
  , workdir   :: Maybe Text
  , verbosity :: Verbosity
}

commonParser :: Parser Common
commonParser =
  Common
    <$> strArgument (metavar "INPUT" <> help "input dhall configuration")
    <*> optional
          (strOption
            (long "workdir" <> metavar "DIRECTORY" <> help
              "working directory (configuration overwrite)"
            )
          )
    <*> flag Normal
             Verbose
             (long "verbose" <> short 'v' <> help "Enable verbose mode")

opts :: Parser (IO ())
opts =
  hsubparser
    $  command
         "run"
         (info (run <$> commonParser) $ progDesc "Run a dhrun specification.")
    <> command
         "print"
         ( info (printY <$> commonParser)
         $ progDesc "print a dhrun specification"
         )
    <> help "Type of operation to run."


data Ext = Dhall | Yaml
ext :: Text -> Maybe Ext
ext fn | xt `elem` [".dh", ".dhall"] = Just Dhall
       | xt `elem` [".yml", ".yaml"] = Just Yaml
       | otherwise                   = Nothing
  where xt = takeExtension $ toS fn

load :: Common -> IO Cfg
load Common {..} = do
  infile <- toS <$> makeAbsolute (toS inputfile)
  overrideV <$> case ext infile of
    (Just Dhall) -> (if v then detailed else identity) $ inputCfg infile
    (Just Yaml ) -> decodeCfg infile
    Nothing      -> die $ "couldn't figure out extension for file " <> infile
 where
  v = verbosity == Verbose
  overrideV x = x
    { DI.verbosity = if (DI.verbosity x == Verbose) || v
                       then Verbose
                       else Normal
    , DI.workdir   = WorkDir $ fromMaybe (toS $ DI.workdir x) workdir
    }

run :: Common -> IO ()
run c = load c >>= DR.runDhrun

printY :: Common -> IO ()
printY c = load c >>= putText . toS . encodeCfg
