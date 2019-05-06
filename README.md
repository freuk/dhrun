`dhrun`
=======

This is a Dhall/YAML configurable job executor meant to be used for
pass/fail CI tests. It starts a list of processes, monitors the standard
streams for patterns that should be expected or avoided, kills the
processes when those criteria are met and exits accordingly.

### Example configurations

See the [examples](.examples/) directory for example `.yml` and `.dh`
configurations.

### Use

You can resolve imports, normalize, and run a dhall configuration on the
fly with:

``` {.bash}
dhrun run path/to/config.dhall 
```

You may also use a plain YAML file:

``` {.bash}
dhrun run path/to/config.yaml
```

The the only notable command-line options are:

-   `dhrun print` which prints the result of a dhall config to stdout
-   the "-" file input argument which forces the binary to use the
    standard input (assuming the YAML format).
-   the `-e` option which allows to edit the yaml configuration on the
    fly before execution

A workflow to evaluate the config via a dhall codebase at configuration
time might involve `dhall` and `dhall-to-yaml`:

``` {.bash}
dhall resolve <<< "let codebase = /path/package.dhall in codebase.foo bar baz" |\
dhall normalize |\
dhall-to-yaml |\
dhrun run "-" 
```

### Installation

    nix-env -f . -iA dhrun

### Dependencies

`dhrun` depends on:

base ansi-terminal protolude mtl bytestring process conduit
unliftio-core containers conduit-extra unix yaml aeson text directory
dhall base protolude directory bytestring editor-open dhrun-lib filepath
optparse-applicative dhall base protolude dhall yaml aeson filepath mtl
bytestring text unliftio tasty tasty-hunit tasty-golden tasty-hspec
tasty-quickcheck generic-random quickcheck-text hspec dhrun-lib Glob

### Hacking

-   `nix-shell` provides a dev environment with cabal-build, ghcid,
    hlint, brittany, and others. There is a small vendored codebase in
    /nix that provides this via nixpkgs pinning.

The structure is the following.

-   `/resources` Dhall types and helper functions.

-   `/app` the main cli tool.

-   `/examples`s example Dhall and YAML configuration files for dhrun.
    These also serve as golden tests.

-   `/src` dhrun types and logic.

-   `/tests` golden, unit, quickcheck tests.

### CLI Help

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
