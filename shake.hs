{-# language OverloadedStrings #-}

{-|
Module      : shake.hs
Description : dev tasks.
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

import           Development.Shake
import           Protolude
import           Development.Shake.FilePath
import           Control.Monad
import           System.Process.Typed
import           System.Posix.Process
import qualified System.IO                     as SIO
                                                ( hSetBuffering
                                                , stdout
                                                , BufferMode(..)
                                                )
import           Options.Applicative           as OA


data GhcidTarget = Test | Lib | App deriving (Enum,Bounded,Show,Read)

toArgs Test = ghcidTarget
  "new-repl test:Tests"
  ["--test=Main.main", "--reload=./resources", "--restart=./src"]
toArgs Lib = ghcidTarget "new-repl dhrun-lib" []
toArgs App = ghcidTarget "new-repl dhrun" []

ghcidTarget :: Text -> [Text] -> [Text]
ghcidTarget target extra =
  [ "--command"
    , "cabal "
    <> target
    , "--restart=dhrun.cabal"
    , "--restart=default.nix"
    , "--restart=shell.nix"
    ]
    ++ extra

runGhcid :: GhcidTarget -> IO ()
runGhcid target = do
  runProcess_ "rm -f .ghc.*"
  executeFile "ghcid" True (toS <$> toArgs target) Nothing

main :: IO ()
main = SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  <> void (join (execParser (info (opts <**> helper) idm)))
 where
  opts :: Parser (IO ())
  opts = hsubparser
    (  OA.command
        "ghcid"
        (info (runGhcid <$> targetParser)
              (progDesc "Run an argo-compatible nix-build.")
        )
    <> OA.command "coverage" (info (pure runcov) (progDesc "run shake."))
    <> OA.command
         "shake"
         (info (pure runshake) (progDesc "run test code coverage."))
    <> OA.command "britt"
                  (info (pure runbritt) (progDesc "inplace brittany."))
    <> help "Type of operation to run."
    )

targetParser :: Parser GhcidTarget
targetParser = argument
  auto
  (metavar "TARGET" <> showDefault <> help
    (toS ("The ghcid target, in " <> mconcat ts))
  )
  where ts = intersperse " " (Prelude.show <$> [(minBound :: GhcidTarget) ..])

runbritt = runProcess_ $ shell
  "brittany --write-mode inplace src/*.hs src/Dhrun/*hs src/Dhrun/Dhrun/*hs"

runcov = do
  runProcess_ "cabal clean"
  runProcess_ "cabal configure --enable-tests --enable-coverage"
  runProcess_ "cabal test"

runshake =
  shakeArgs shakeOptions $ phony "clean" $ removeFilesAfter "." ["README.md"]
