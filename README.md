`dhrun`
=======

This is a Dhall/Yaml-based job executor meant to be used for pass/fail CI
tests.  It monitors the standard streams for patterns that should be expected
or avoided, kills the processes when those criteria are met and exits
accordingly.

### Use

You can resolve imports, normalize, and run a dhall configuration on the fly with:
```
dhrun run path/to/config.dhall 
```

You may also use a plain YAML file:
```
dhrun run path/to/config.yaml
```

The the only notable command-line options are:

- `dhrun print` which prints the result of a dhall config to stdout
- the "-" file input argument which forces the binary to use the standard input
  (assuming the YAML format).
- the `-e` option which allows to edit the yaml configuration on the fly before
  execution

A workflow to evaluate the config via a dhall codebase at configuration time
might involve `dhall` and `dhall-to-yaml`:

```
dhall resolve <<< "let codebase = /path/package.dhall in codebase.foo bar baz" |\
dhall normalize |\
dhall-to-yaml |\
dhrun run "-" 
```

### Installation

```
nix-env -f . -iA dhrun
```

### Hacking

- `nix-shell` provides a dev environment with cabal-build, ghcid, hlint,
  brittany, and others. There is a small vendored codebase in /nix that
  provides this via nixpkgs pinning.  

The structure is the following.
- `/resources` Dhall types and helper functions.
- `/app` the main cli tool.
- `/examples`s example Dhall and YAML configuration files for dhrun. These
  also serve as golden tests.
- `/src` dhrun types and logic.
- `/tests` golden, unit, quickcheck tests.
