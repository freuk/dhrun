{-# LANGUAGE OverloadedStrings #-}
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

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $ info
  (helper <*> opts)
  (fullDesc <> header "Dhall-based threaded executor" <> progDesc
    ("This small program allows to configure and execute asynchronous processes."
    <> "It is meant to be used for CI jobs."
    )
  )

data Common = Common
  { inputfile :: Text
  , verbosity :: Verbosity
}

commonParser :: Parser Common
commonParser =
  Common
    <$> (("./" <>) <$> strArgument
          (metavar "INPUT" <> help "input dhall configuration")
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
load c = overrideV <$> case ext (inputfile c) of
  (Just Dhall) ->
    (if Main.verbosity c == Verbose then detailed else identity)
      $ inputCfg
      $ inputfile c
  (Just Yaml) -> decodeCfg $ inputfile c
  Nothing     -> die $ "couldn't figure out extension for file " <> inputfile c
 where
  overrideV x = x
    { DI.verbosity =
      if (DI.verbosity x == Verbose) || (Main.verbosity c == Verbose)
        then Verbose
        else Normal
    }

run :: Common -> IO ()
run c = load c >>= DR.runDhrun

printY :: Common -> IO ()
printY c = load c >>= putText . toS . encodeCfg
