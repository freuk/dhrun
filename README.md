##### `dhrun`: [Dhall](https://dhall-lang.org/)/[YAML](https://yaml.org/)/JSON configurable concurrent process executor with streaming assertions.

`dhrun` starts a list of (Unix) processes, monitors the standard streams for
patterns that should be expected or avoided, kills the processes when criteria
are met and exits accordingly. It is configured using either
[Dhall](https://dhall-lang.org/), YAML or JSON.  Its goals are similar to
[venom](https://github.com/ovh/venom). Compared to that tool, `dhrun` has only
one execution capability(exec) and its assertions are poor(infix strings only).
It supports concurrency and monitors streams, however. It was written to create
a configuration layer to control integration tests where multiple processes have
to interact.

##### Use

``` {.bash}
dhrun path/to/config.dhall 
dhrun path/to/config.yaml
dhrun path/to/config.json
```

 The [resources](./resources) directory contains the Dhall types for the
 configuration layer, and the [examples](./examples/) directory contains more
 `.yaml` and `.dh` configurations.

The originally intended workflow is to configure dhrun using a dhall
codebase. In bash, this might involve a here-document:

``` {.bash}
dhrun -i <<< "let codebase = /path/package.dhall in codebase.foo bar baz"
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
dhrun --help
```

``` {.txt}
dhrun

Usage: dhrun [-i|--stdin] [CONFIG] [-y|--yaml]
  Dhrun

Available options:
  -h,--help                Show this help text
  -i,--stdin               Read configuration on stdin.
  CONFIG                   Input configuration with .yml/.yaml/.dh/.dhall
                           extension. Leave void for stdin (dhall) input.
  -y,--yaml                Assume configuration to be yaml(json is valid yaml)
                           instead of dhall.
  -h,--help                Show this help text
```

##### Installation

-   get [Nix](https://nixos.org/nix/)

-   install the 1.0.0 release:
    `nix-env -f https://github.com/freuk/dhrun/archive/1.0.0.tar.gz -iA dhrun`

This pins nixpkgs 18.09. Building should take a minute or two and will
definitely succeed. You can also try installing the bleeding edge
version: `nix-env -f https://github.com/freuk/dhrun.git -iA dhrun`

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

##### See also

-   [venom](https://github.com/ovh/venom)
-   [tasty-program](http://hackage.haskell.org/package/tasty-program)
