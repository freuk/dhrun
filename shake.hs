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

refactCommands =
  [ "OrganizeImports Dhallexec.Stack"
  , "OrganizeImports Dhallexec.Args"
  , "OrganizeImports Dhallexec.Utils"
  , "OrganizeImports Dhallexec.Types"
  ]

main = getArgs >>= deal
 where
  deal args
    | "ghcid" `elem` args = void $ startProcess $ proc
      "ghcid"
      [ "--command"
      , "cabal new-repl "
      <> " --ghc-options=-fno-code"
      <> " --ghc-options=-fno-break-on-exception"
      <> " --ghc-options=-fno-break-on-error"
      <> " --ghc-options=-v1 --ghc-options=-ferror-spans"
      , "--restart"
      , "argonix.cabal"
      , "--restart"
      , "default.nix"
      , "--restart"
      , "shell.nix"
      ]
    | otherwise = runshake

runshake = shakeArgs shakeOptions $ do
  phony "clean" $ removeFilesAfter "." ["README.md"]

  phony "ht-refact" htRefactAll
  phony "brittany"  brittany
  phony "codequality" $ htRefactAll >> brittany

  want ["README.md"]

  "README.md" %> \out -> do
    let template = ".README.md"
    need [template, "src/argotk.hs", "src/Dhallexec/Stack.hs"]
    (Stdout panpipe) <- cmd "which panpipe"
    cmd_ "pandoc --filter"
         [take (length panpipe - 1) panpipe, template, "-o", out]
 where
  brittany =
    cmd_ Shell $ "brittany --write-mode inplace" <> " src/*.hs src/Dhallexec/*hs"
  htRefactAll = for_ refactCommands htRefact
  htRefact x =
    cmd_ Shell
      $  "ht-refact "
      <> "-w `which hfswatch` "
      <> "--project-type cabal "
      <> ". "
      <> "-e \""
      <> x
      <> "\" || true"
