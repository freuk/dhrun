`dhrun`
=======

This is a Dhall/YAML configurable job executor meant to be used for
pass/fail CI tests. It starts a list of processes, monitors the standard
streams for patterns that should be expected or avoided, kills the
processes when those criteria are met and exits accordingly.

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

See the [examples](./examples/) directory for example `.yml` and `.dh`
configurations. The [resources](./resources) directory contains the
Dhall types for the configuration layer. It also serves as reference
documentation, but here's a yaml example to serve as a quickstart:

``` {.yaml}
verbose: null # whether to be verbose.
cleaning: null # whether to remove the global workdir on startup.
workdir: /tmp/dhrun # the global working directory (with creation) - defaults to "./"
pre: # a list of shell commands to run before the asynchronous step.
  - pwd # will print /tmp/dhrun
cmds: # a list of commands with the following structure:
- otherwd: null # an alternate working directory for this command
  name: echo # command name 
  args: # arguments
    - hello
    - world
  # a "filecheck" on out: terminates dhrun successfully as soon as all 'wants'
  # are observed on *at least one* command, and exits dhrun with an error as soon 
  # as one "avoid" is observed. These specifications are substrings to be matched.
  out:
    filecheck:
      wants: 
       - hello
       - world
      avoids: null
    filename: foo
  err: # a "filecheck" on stderr
    filecheck:
      wants: null
      avoids:
       - Traceback
    filename: bar
  postchecks: null # a list of "filechecks" to be performed post-execution
  passvars: null # environment variables to inherit (famously, PATH.)
  vars: # environment variables to enforce 
  - value: BAR
    varname: FOO
  timeout: 4 # timeout in seconds
post: # a list of shell commands to run after the asynchronous step.
```

Useful command-line options are the following.

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
