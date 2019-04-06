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
import           Dhrun.Types
import           Options.Applicative           as OA
import           Dhall

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $ info
  (helper <*> parser)
  (fullDesc <> header "Dhall-based threaded executor" <> progDesc
    ("This small program allows to configure and execute asynchronous processes."
    <> "It is meant to be used for CI jobs."
    )
  )

parser :: Parser (IO ())
parser =
  work
    <$> strOption
          (long "input" <> short 'i' <> metavar "PATH" <> help
            "input dhall configuration"
          )
    <*> flag Normal
             Verbose
             (long "verbose" <> short 'v' <> help "Enable verbose mode")

work :: Text -> Verbosity -> IO ()
work fn v = do
  loaded <- (if v == Verbose then detailed else identity) $ input ft fn
  print loaded
 where
  ft :: Dhall.Type DhallExec
  ft = Dhall.auto
