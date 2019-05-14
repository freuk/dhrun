`dhrun`
=======

This is a Dhall/YAML configurable concurrent job executor meant to be
used for pass/fail CI tests. It starts a list of processes, monitors the
standard streams for patterns that should be expected or avoided, kills
the processes when those criteria are met and exits accordingly. This is
in the spirit of [venom](https://github.com/ovh/venom). Compared to that
tool, dhrun has only one execution capability and its assertion
specifications are poor. It supports concurrency, however.

### Use

Resolve imports, normalize, and run a dhrun configuration on the fly
with:

``` {.bash}
dhrun run path/to/config.dhall 
```

Or use YAML:

``` {.bash}
dhrun run path/to/config.yaml
```

See the [examples](./examples/) directory for example `.yml` and `.dh`
configurations. The [resources](./resources) directory contains the
Dhall types for the configuration layer. File
[quickstart.yml](./quickstart.yml) serves as a quickstart example.

Useful command-line options are the following:

-   `dhrun print` prints the result of a dhall config to stdout
-   The `"-"` file input argument forces the binary to use the standard
    input (assuming the YAML format).
-   The `-e` option allows to edit the configuration (in YAML) on the
    fly before execution

A workflow to evaluate the config via a dhall codebase at configuration
time might involve `dhall` and `dhall-to-yaml`:

``` {.bash}
dhall resolve <<< "let codebase = /path/package.dhall in codebase.foo bar baz" |\
dhall normalize |\
dhall-to-yaml |\
dhrun run "-" 
```

### CLI Interface

``` {.hidden}
dhrun --help
```

``` {.txt}
dhrun

Usage: dhrun COMMAND
  dhrun is a bare-bones dhall/yaml-configured asynchronous process executor that
  features configurable streaming successs/failure behaviors based on pattern
  matches on stdout/stderr.

Available options:
  -h,--help                Show this help text
  COMMAND                  Type of operation to run.

Available commands:
  run                      Run a dhrun specification.
  print                    print a dhrun specification
```

``` {.bash}
dhrun run --help
```

``` {.txt}
Usage: dhrun run INPUT [--workdir DIRECTORY] [-v|--verbose] [-e|--edit]
  Run a dhrun specification.

Available options:
  INPUT                    input dhall configuration
  --workdir DIRECTORY      working directory (configuration overwrite)
  -v,--verbose             Enable verbose mode
  -e,--edit                Edit yaml before run
  -h,--help                Show this help text
```

### Installation

There is a small vendored Nix codebase in /nix.

    nix-env -f . -iA dhrun

The `/nix` folder is mostly there to manage the development workflow so
feel free to try other ways. The hackage dependencies for `dhrun` and
its tests are:

`base` `ansi-terminal` `protolude` `mtl` `bytestring` `process`
`conduit` `unliftio-core` `containers` `conduit-extra` `unix` `yaml`
`aeson` `text` `directory` `dhall` `base` `protolude` `directory`
`bytestring` `editor-open` `dhrun-lib` `filepath` `optparse-applicative`
`dhall` `base` `protolude` `dhall` `yaml` `aeson` `filepath` `mtl`
`bytestring` `text` `unliftio` `tasty` `tasty-hunit` `tasty-golden`
`tasty-hspec` `tasty-quickcheck` `generic-random` `quickcheck-text`
`hspec` `dhrun-lib` `Glob`

### Hacking

-   `nix-shell` provides a dev environment with cabal-build, ghcid,
    hlint, brittany, and others. There is a small vendored codebase in
    /nix that provides this via nixpkgs pinning.

-   you need to procure
    [`dhall-to-cabal`](https://github.com/dhall-lang/dhall-to-cabal)
    separately because I didn't see fit to add that package as of the
    shell environment. It's necessary if you want to edit the cabal
    file, which is done through [`./cabal.dh`](./cabal.dh).

-   edit `.README.md` instead of `README.md` and run `./shake readme`.
    `./shake` also has other useful dev workflows.

-   `direnv allow` for [lorri](https://github.com/target/lorri)
    integration.

The structure of the code is the following.

-   `/resources` Dhall types and helper functions.

-   `/app` the main cli tool.

-   `/examples`s example Dhall and YAML configuration files for dhrun.
    These also serve as golden tests.

-   `/src` dhrun types and logic.

-   `/tests` golden, unit, quickcheck tests.
