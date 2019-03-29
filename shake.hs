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

data GhcidTargets = Test | Lib | App
toProc Test = ghcidTarget
  "new-repl test:Tests"
  ["--test=Main.main", "--restart", "examples/singleProcess.dh"]
toProc Lib = ghcidTarget "new-repl dhall-exec-lib" []
toProc App = ghcidTarget "new-repl dhall-exec" []

ghcidTarget :: Text -> [Text] -> ProcessConfig () () ()
ghcidTarget target extra =
  proc "ghcid"
    $   toS
    <$> (  [ "--command"
           , "cabal "
           <> target
           <> " "
           <> " --ghc-options=-fno-code"
           <> " --ghc-options=-fno-break-on-exception"
           <> " --ghc-options=-fno-break-on-error"
           <> " --ghc-options=-v1 --ghc-options=-ferror-spans"
           , "--restart"
           , "dhall-exec.cabal"
           , "--restart"
           , "default.nix"
           , "--restart"
           , "shell.nix"
           ]
        ++ extra
        )

main = do
  runProcess_ "rm -f .ghc.*"
  getArgs >>= deal
 where
  deal args | "ghcid-test" `elem` args = void $ startProcess (toProc Test)
            | "ghcid-lib" `elem` args  = void $ startProcess (toProc Lib)
            | "ghcid-app" `elem` args  = void $ startProcess (toProc App)
            | otherwise                = runshake
    where libGhcid = ghcidTarget "new-repl dhall-exec-lib" []


runshake = shakeArgs shakeOptions $ do
  phony "clean" $ removeFilesAfter "." ["README.md"]

  phony "brittany" brittany

  want ["README.md"]

  "README.md" %> \out -> do
    let template = ".README.md"
    need [template, "src/argotk.hs", "src/Dhallexec/Stack.hs"]
    panpipe <- toS <$> readProcessStdout_ "which panpipe"
    runProcess_ $ proc
      "pandoc"
      ["--filter", take (length panpipe - 1) panpipe, template, "-o", out]
 where
  brittany = runProcess_
    $ shell "brittany --write-mode inplace src/*.hs src/Dhallexec/*hs"
