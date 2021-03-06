let prelude = ./nix/dhall2cabal/dhall-to-cabal/prelude.dhall

let types = ./nix/dhall2cabal/dhall-to-cabal/types.dhall

let defexts =
      [ types.Extension.LambdaCase True
      , types.Extension.QuasiQuotes True
      , types.Extension.DefaultSignatures True
      , types.Extension.ExistentialQuantification True
      , types.Extension.RecordWildCards True
      , types.Extension.TypeSynonymInstances True
      , types.Extension.StandaloneDeriving True
      , types.Extension.FlexibleInstances True
      , types.Extension.TupleSections True
      , types.Extension.MultiParamTypeClasses True
      , types.Extension.ImplicitPrelude False
      , types.Extension.OverloadedStrings True
      , types.Extension.ViewPatterns True
      , types.Extension.DeriveFunctor True
      , types.Extension.DeriveTraversable True
      , types.Extension.TypeFamilies True
      , types.Extension.DeriveAnyClass True
      , types.Extension.DeriveGeneric True
      , types.Extension.DeriveDataTypeable True
      , types.Extension.DeriveFoldable True
      , types.Extension.DerivingStrategies True
      , types.Extension.TypeApplications True
      , types.Extension.MultiWayIf True
      , types.Extension.TemplateHaskell False
      , types.Extension.BlockArguments True
      , types.Extension.GADTs True
      , types.Extension.FlexibleContexts True
      , types.Extension.TypeOperators True
      , types.Extension.DataKinds True
      , types.Extension.PolyKinds True
      , types.Extension.AllowAmbiguousTypes True
      , types.Extension.FunctionalDependencies True
      , types.Extension.UndecidableInstances True
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
          nobound "base"
      , bytestring =
          nobound "bytestring"
      , containers =
          nobound "containers"
      , text =
          nobound "text"
      , unix =
          nobound "unix"
      , lens =
          nobound "lens"
      , generic-lens =
          nobound "generic-lens"
      , time =
          nobound "time"
      , ansi-terminal =
          nobound "ansi-terminal"
      , conduit =
          nobound "conduit"
      , directory =
          nobound "directory"
      , aeson-extra =
          nobound "aeson-extra"
      , aeson-pretty =
          nobound "aeson-pretty"
      , mtl =
          nobound "mtl"
      , unliftio-core =
          nobound "unliftio-core"
      , conduit-extra =
          nobound "conduit-extra"
      , process =
          nobound "process"
      , dhall =
          nobound "dhall"
      , protolude =
          nobound "protolude"
      , yaml =
          nobound "yaml"
      , neat-interpolation =
          nobound "neat-interpolation"
      , filepath =
          nobound "filepath"
      , optparse-applicative =
          nobound "optparse-applicative"
      , editor-open =
          nobound "editor-open"
      , dhall-json =
          nobound "dhall-json"
      , data-default =
          nobound "data-default"
      , tasty =
          nobound "tasty"
      , aeson =
          nobound "aeson"
      , unliftio =
          nobound "unliftio"
      , prettyprinter =
          nobound "prettyprinter"
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
          "`dhrun` starts a list of (Unix) processes, monitors the standard streams for patterns that should be expected or avoided, kills the processes when criteria are met and exits accordingly. It is configured using either [Dhall](https://dhall-lang.org/),YAML or JSON. See the [README.md](https://github.com/freuk/dhrun) file for details."
      , executables =
          [ { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Dhrun.hs"
                    , build-depends =
                        [ nobound "dhrun-lib", deps.protolude ]
                    , hs-source-dirs =
                        [ "bin" ]
                    }
                  ⫽ copts [ "-threaded" ]
            , name =
                "dhrun"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Codegen.hs"
                    , build-depends =
                        [ nobound "dhrun-lib", deps.protolude ]
                    , hs-source-dirs =
                        [ "bin" ]
                    }
                  ⫽ copts ([] : List Text)
            , name =
                "codegen"
            }
          ]
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
                        , deps.directory
                        , deps.filepath
                        , deps.dhall-json
                        , deps.dhall
                        , deps.data-default
                        , deps.optparse-applicative
                        , deps.protolude
                        , deps.containers
                        , deps.text
                        , deps.aeson-pretty
                        , deps.aeson
                        , deps.lens
                        , deps.generic-lens
                        , deps.aeson-extra
                        , deps.unix
                        , deps.time
                        , deps.ansi-terminal
                        , deps.prettyprinter
                        , deps.conduit
                        , deps.mtl
                        , deps.neat-interpolation
                        , deps.unliftio-core
                        , deps.conduit-extra
                        , deps.process
                        , deps.yaml
                        ]
                    , hs-source-dirs =
                        [ "src" ]
                    , exposed-modules =
                        [ "Dhrun.Types.Cfg"
                        , "Dhrun.Run"
                        , "Dhrun.Pure"
                        , "Dhrun.Conduit"
                        , "Dhrun.Bin"
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
                        , deps.directory
                        , deps.aeson
                        , deps.filepath
                        , deps.mtl
                        , deps.bytestring
                        , deps.text
                        , deps.unliftio
                        , deps.aeson-extra
                        , deps.data-default
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
