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

data GhcidTargets = Test | Lib | App
toArgs Test = ghcidTarget
  "new-repl test:Tests"
  [ "--test=Main.main"
  , "--reload=./resources"
  , "--reload=./examples"
  , "--restart=./src"
  ]
toArgs Lib = ghcidTarget "new-repl dhrun-lib" []
toArgs App = ghcidTarget "new-repl dhrun" []

ghcidTarget :: Text -> [Text] -> [Text]
ghcidTarget target extra =
  [ "--command"
    , "cabal "
    <> target
    <> " "
    <> " --ghc-options=-fno-code"
    <> " --ghc-options=-fno-break-on-exception"
    <> " --ghc-options=-fno-break-on-error"
    <> " --ghc-options=-v1 --ghc-options=-ferror-spans"
    , "--restart=dhrun.cabal"
    , "--restart=default.nix"
    , "--restart=shell.nix"
    ]
    ++ extra

main = do
  runProcess_ "rm -f .ghc.*"
  getArgs >>= deal
 where
  deal args
    | "ghcid-test" `elem` args = void
    $ executeFile "ghcid" True (toS <$> toArgs Test) Nothing
    | "ghcid-lib" `elem` args = void
    $ executeFile "ghcid" True (toS <$> toArgs Lib) Nothing
    | "ghcid-app" `elem` args = void
    $ executeFile "ghcid" True (toS <$> toArgs App) Nothing
    | otherwise = runshake

runshake = shakeArgs shakeOptions $ do
  phony "clean" $ removeFilesAfter "." ["README.md"]

  phony "brittany" brittany

  {-want ["README.md"]-}

  {-"README.md" %> \out -> do-}
    {-let template = ".README.md"-}
    {-need [template, "src/Dhrun.hs", "src/Dhrun/Types.hs", "src/Dhrun/Run.hs"]-}
    {-panpipe <- toS <$> readProcessStdout_ "which panpipe"-}
    {-runProcess_ $ proc-}
      {-"pandoc"-}
      {-["--filter", take (length panpipe - 1) panpipe, template, "-o", out]-}
 where
  brittany = runProcess_
    $ shell "brittany --write-mode inplace src/*.hs src/Dhallexec/*hs"
