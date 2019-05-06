{-# language OverloadedStrings #-}
{-# language PackageImports #-}

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
import qualified System.IO                                         as SIO
                   ( hSetBuffering
                   , stdout
                   , BufferMode(..)
                   )
import           Options.Applicative                               as OA
import           System.Directory
import "Glob"    System.FilePath.Glob
import           System.Environment


data GhcidTarget = Test | Lib | App deriving (Enum,Bounded,Show,Read)

toArgs Test = ghcidTarget
  "new-repl test:Tests"
  ["--test=Main.main", "--reload=./resources", "--restart=./src"]
toArgs Lib = ghcidTarget "new-repl dhrun-lib" []
toArgs App = ghcidTarget "new-repl dhrun" []

ghcidTarget :: Text -> [Text] -> [Text]
ghcidTarget target extra =
  [ "--command"
    , "cabal " <> target
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
    <> OA.command "britt"
                  (info (pure runbritt) (progDesc "inplace brittany."))
    <> OA.command "cabal"
                  (info (pure cabal) (progDesc "generate cabal file."))
    <> OA.command "coverage"
                  (info (pure runcov) (progDesc "run code coverage"))
    <> OA.command "shake" (info (pure (runshake [])) (progDesc "run shake."))
    <> OA.command
         "readme"
         (info (pure (runshake ["README.md"])) (progDesc "run shake."))
    <> OA.command
         "build"
         (info (pure (runshake ["build"])) (progDesc "run shake."))
    <> help "Type of operation to run."
    )

targetParser :: Parser GhcidTarget
targetParser = argument
  auto
  (metavar "TARGET" <> showDefault <> help
    (toS ("The ghcid target, in " <> mconcat ts))
  )
  where ts = intersperse " " (Prelude.show <$> [(minBound :: GhcidTarget) ..])

runbritt =
  mapM glob ["*.hs", "*/*.hs", "*/*/*.hs", "*/*/*.hs"] <&> concat >>= mapM_
    (\fn -> runProcess_ $ shell ("brittany --write-mode inplace " <> toS fn))

cabal = runProcess_ $ shell "dhall-to-cabal ./cabal.dh"

runcov = do
  runProcess_ "cabal clean"
  runProcess_ "cabal configure --enable-tests --enable-coverage"
  runProcess_ "cabal test"

runshake as = withArgs as $ shakeArgs shakeOptions $ do
  want ["README.md", "dist/buisd/dhrun"]

  phony "clean" $ removeFilesAfter "REAMDE.md" ["."]

  phony "build" $ liftIO (runProcess_ $ proc "cabal" ["build"])

  ".README.out.md" %> \out -> do
    let template = ".README.md"
    need [template]
    panpipe <- toS <$> liftIO (readProcessStdout_ $ shell "which panpipe")
    putText $ toS panpipe
    liftIO
      (runProcess_ $ proc
        "pandoc"
        ["--filter", take (length panpipe - 1) panpipe, template, "-o", out]
      )

  "README.md" %> \out -> do
    let template = ".README.out.md"
    need [template]
    panhandle <- toS <$> liftIO (readProcessStdout_ $ shell "which panhandle")
    putText $ toS panhandle
    liftIO
      (runProcess_ $ proc
        "pandoc"
        ["--filter", take (length panhandle - 1) panhandle, template, "-o", out]
      )
