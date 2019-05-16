##### `dhrun`: [Dhall](https://dhall-lang.org/)/[YAML](https://yaml.org/) configurable concurrent process executor with streaming assertions.

`dhrun` starts a list of (Unix) processes, monitors the standard streams
for patterns that should be expected or avoided, kills the processes
when criteria are met and exits accordingly. It is configured using
either [Dhall](https://dhall-lang.org/) or [YAML](https://yaml.org/).
Its goals are similar to [venom](https://github.com/ovh/venom). Compared
to that tool, `dhrun` has only one execution capability(exec) and its
assertions are poor(infix strings only). It supports concurrency and
monitors streams, however. It was written to create a configuration
layer to control single-node integration tests for a linux daemon.

##### Use

``` {.bash}
dhrun run path/to/config.dhall 
dhrun run path/to/config.yaml
```

See file [quickstart.yml](./quickstart.yml) for an overview of `dhrun`'s
capabilities. The [resources](./resources) directory contains the Dhall
types for the configuration layer, and the [examples](./examples/)
directory contains more `.yml` and `.dh` configurations.

The originally intended workflow is to configure dhrun using a dhall
codebase. In bash, this might involve a here document:

``` {.bash}
dhrun run <<< "let codebase = /path/package.dhall in codebase.foo bar baz"
```

You can find an example project-specific dhall configuration layer
[here](https://xgitlab.cels.anl.gov/argo/argopkgs/blob/master/dhrun/all-tests.dh).

##### CLI Interface

``` {.hidden}
dhrun --help
```

``` {.txt}
dhrun

Usage: dhrun COMMAND
  dhall-configured concurrent process execution with streaming assertion
  monitoring

Available options:
  -h,--help                Show this help text
  COMMAND                  Type of operation to run.

Available commands:
  run                      Run a dhrun specification.
  print                    Print a dhrun specification.
```

``` {.bash}
dhrun run --help
```

``` {.txt}
Usage: dhrun run [INPUT] [-y|--yaml] [-v|--verbose] [-e|--edit]
  Run a dhrun specification.

Available options:
  INPUT                    Input configuration with .yml/.yaml/.dh/.dhall
                           extension. Leave void for stdin (dhall) input.
  -y,--yaml                Assume stdin to be yaml instead of dhall.
  -v,--verbose             Enable verbose mode.
  -e,--edit                Edit yaml in $EDITOR before run.
  -h,--help                Show this help text
```

##### Installation

-   get [Nix](https://nixos.org/nix/)

-   install 1.0.0 release:
    `nix-env -f https://github.com/freuk/dhrun/archive/1.0.0.tar.gz -iA dhrun`

-   install bleeding edge:
    `nix-env -f https://github.com/freuk/dhrun.git -iA dhrun`

This pins nixpkgs 18.09. Building should take a minute or two and will
definitely succeed.

I have not tried building this code using stack or any other tool, but
here are the hackage dependencies for `dhrun` and its tests:

`base` `ansi-terminal` `protolude` `mtl` `bytestring` `process`
`conduit` `unliftio-core` `containers` `conduit-extra` `unix` `yaml`
`aeson` `text` `directory` `dhall` `base` `protolude` `directory`
`bytestring` `editor-open` `dhrun-lib` `filepath` `optparse-applicative`
`dhall` `base` `protolude` `dhall` `yaml` `aeson` `filepath` `mtl`
`bytestring` `text` `unliftio` `tasty` `tasty-hunit` `tasty-golden`
`tasty-hspec` `tasty-quickcheck` `generic-random` `quickcheck-text`
`hspec` `dhrun-lib` `Glob`

##### Hacking

-   `nix-shell` provides a dev environment with cabal-build, ghcid,
    hlint, brittany, and other tools.

-   you need to procure
    [`dhall-to-cabal`](https://github.com/dhall-lang/dhall-to-cabal)
    separately. It's necessary if you want to edit the cabal file, which
    is done through [`./cabal.dh`](./cabal.dh).

-   edit `.README.md` instead of `README.md` and run `./shake readme`.
    `./shake` also has other useful dev workflows.

-   `direnv allow` for [lorri](https://github.com/target/lorri)
    integration.

-   use the `./pre-commit.sh` hook.

##### Structure

-   `/nix` vendored nix expressions.

-   `/resources` Dhall types and helper functions.

-   `/app` the main cli tool.

-   `/examples` example Dhall and YAML configuration files for dhrun.
    These also serve as golden tests.

-   `/src` dhrun types and logic.

-   `/tests` golden, unit, quickcheck tests.
