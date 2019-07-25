let prelude = ./cabal/prelude.dhall

let types = ./cabal/types.dhall

let defexts =
      [ types.Extension.LambdaCase True
      , types.Extension.RecordWildCards True
      , types.Extension.ScopedTypeVariables True
      , types.Extension.ImplicitPrelude False
      , types.Extension.OverloadedStrings True
      , types.Extension.ViewPatterns True
      ]

let deflang = Some types.Language.Haskell2010

let defcopts =
        λ(addcopts : List Text)
      →   prelude.defaults.CompilerOptions
        ⫽ { GHC =
                  [ "-Wall"
                  , "-Wcompat"
                  , "-Wincomplete-uni-patterns"
                  , "-Wincomplete-record-updates"
                  , "-Wmissing-home-modules"
                  , "-Widentities"
                  , "-Wredundant-constraints"
                  , "-Wcpp-undef"
                  , "-fwarn-tabs"
                  , "-fwarn-unused-imports"
                  , "-fwarn-missing-signatures"
                  , "-fwarn-name-shadowing"
                  , "-fprint-potential-instances"
                  , "-Wmissing-export-lists"
                  , "-fwarn-unused-do-bind"
                  , "-fwarn-wrong-do-bind"
                  , "-fwarn-incomplete-patterns"
                  ]
                # addcopts
              : List Text
          }

let copts =
        λ(addcopts : List Text)
      → { compiler-options =
            defcopts addcopts
        , default-extensions =
            defexts
        , default-language =
            deflang
        }

let nobound = λ(p : Text) → { bounds = prelude.anyVersion, package = p }

let genbounds =
        λ(p : Text)
      → λ(lo : Text)
      → λ(hi : Text)
      → { bounds =
            prelude.intersectVersionRanges
            ( prelude.unionVersionRanges
              (prelude.thisVersion (prelude.v lo))
              (prelude.laterVersion (prelude.v lo))
            )
            (prelude.earlierVersion (prelude.v hi))
        , package =
            p
        }

let deps =
      { dhrun-lib =
          nobound "dhrun-lib"
      , base =
          genbounds "base" "4.11.1" "4.12"
      , bytestring =
          genbounds "bytestring" "0.10.8" "0.11"
      , containers =
          genbounds "containers" "0.5.11" "0.6"
      , text =
          genbounds "text" "1.2.3" "1.3"
      , unix =
          genbounds "unix" "2.7.2" "2.8"
      , time =
          genbounds "time" "1.8.0" "1.9"
      , ansi-terminal =
          genbounds "ansi-terminal" "0.9.1" "0.10"
      , conduit =
          genbounds "conduit" "1.3.0" "1.4"
      , directory =
          genbounds "directory" "1.3.1" "1.4"
      , mtl =
          genbounds "mtl" "2.2.2" "2.3"
      , unliftio-core =
          genbounds "unliftio-core" "0.1.2" "0.2"
      , conduit-extra =
          genbounds "conduit-extra" "1.3.0" "1.4"
      , process =
          genbounds "process" "1.6.3" "1.7"
      , dhall =
          genbounds "dhall" "1.24.0" "1.25"
      , protolude =
          genbounds "protolude" "0.2.2" "0.3"
      , yaml =
          genbounds "yaml" "0.8.32" "0.9"
      , filepath =
          genbounds "filepath" "1.4.2" "1.5"
      , optparse-applicative =
          genbounds "optparse-applicative" "0.15.0" "0.16"
      , editor-open =
          genbounds "editor-open" "0.6.0" "0.7"
      , tasty =
          nobound "tasty"
      , aeson =
          nobound "aeson"
      , unliftio =
          nobound "unliftio"
      , tasty-hunit =
          nobound "tasty-hunit"
      , tasty-golden =
          nobound "tasty-golden"
      , tasty-hspec =
          nobound "tasty-hspec"
      , tasty-quickcheck =
          nobound "tasty-quickcheck"
      , generic-random =
          nobound "generic-random"
      , quickcheck-text =
          nobound "quickcheck-text"
      , hspec =
          nobound "hspec"
      , glob =
          nobound "Glob"
      }

in    prelude.defaults.Package
    ⫽ { name =
          "dhrun"
      , version =
          prelude.v "1.0.1"
      , author =
          "Valentin Reis"
      , build-type =
          Some types.BuildType.Simple
      , cabal-version =
          prelude.v "2.0"
      , category =
          "tools"
      , description =
          "`dhrun` starts a list of (Unix) processes, monitors the standard streams for patterns that should be expected or avoided, kills the processes when criteria are met and exits accordingly. It is configured using either [Dhall](https://dhall-lang.org/) or [YAML](https://yaml.org/). See the [README.md](https://github.com/freuk/dhrun) file for details."
      , executables =
          [ { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Main.hs"
                    , build-depends =
                        [ deps.base
                        , deps.bytestring
                        , deps.directory
                        , deps.filepath
                        , deps.dhall
                        , deps.optparse-applicative
                        , deps.protolude
                        , deps.editor-open
                        ]
                    , hs-source-dirs =
                        [ "app" ]
                    }
                  ⫽ copts [ "-threaded" ]
            , name =
                "dhrun"
            }
          ]
      , extra-source-files =
          [ "ChangeLog.md" ]
      , license =
          types.License.MIT
      , license-files =
          [ "LICENSE" ]
      , maintainer =
          "fre@freux.fr"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { type =
                  Some types.RepoType.Git
              , location =
                  Some "https://github.com/freuk/dhrun"
              }
          ]
      , sub-libraries =
          [ { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        [ deps.base
                        , deps.bytestring
                        , deps.containers
                        , deps.text
                        , deps.unix
                        , deps.time
                        , deps.ansi-terminal
                        , deps.conduit
                        , deps.directory
                        , deps.mtl
                        , deps.unliftio-core
                        , deps.conduit-extra
                        , deps.process
                        , deps.dhall
                        , deps.protolude
                        , deps.yaml
                        ]
                    , hs-source-dirs =
                        [ "src" ]
                    , exposed-modules =
                        [ "Dhrun.Types.Cfg"
                        , "Dhrun.Types.Dhall"
                        , "Dhrun.Types.Yaml"
                        , "Dhrun.Run"
                        , "Dhrun.Pure"
                        , "Dhrun.Conduit"
                        ]
                    }
                  ⫽ copts ([] : List Text)
            , name =
                "dhrun-lib"
            }
          ]
      , synopsis =
          "Dhall/YAML configurable concurrent integration test executor."
      , test-suites =
          [ { name =
                "Tests"
            , test-suite =
                  λ(config : types.Config)
                →   prelude.defaults.TestSuite
                  ⫽ { type =
                        types.TestType.exitcode-stdio { main-is = "Tests.hs" }
                    , build-depends =
                        [ deps.base
                        , deps.protolude
                        , deps.dhall
                        , deps.yaml
                        , deps.aeson
                        , deps.filepath
                        , deps.mtl
                        , deps.bytestring
                        , deps.text
                        , deps.unliftio
                        , deps.tasty
                        , deps.tasty-hunit
                        , deps.tasty-golden
                        , deps.tasty-hspec
                        , deps.tasty-quickcheck
                        , deps.generic-random
                        , deps.quickcheck-text
                        , deps.hspec
                        , deps.dhrun-lib
                        , deps.glob
                        ]
                    , hs-source-dirs =
                        [ "tests" ]
                    }
                  ⫽ copts [ "-threaded" ]
            }
          ]
      }
