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

This also works with a YAML file:
```
dhrun run path/to/config.yaml
```

The the only notable command-line options are `dhrun print` which print the
result of a dhall config to stdout, the `-e` option which allows to edit the
yaml configuration on the fly before execution, and the "-" file input argument
which forces the binary to use the standard input (assuming the YAML format).

A workflow to evaluate the config via a dhall codebase at configuration time
would involve `dhall` and `dhall-to-yaml`.

```
dhall resolve <<< "let codebase = /path/package.dhall in codebase.foo bar baz" |\
dhall normalize |\
dhall-to-yaml |\
dhrun run "-" 
```

### Installation

Only Nix is provided.
```
nix-env -f . -iA dhrun
```

### Hacking

- `nix-shell` provides a dev environment with cabal-build, ghcid, hlint,
  brittany, and others. There is a small vendored codebase in /nix that
  provides this via nixpkgs pinning.  

The structure is the following.
- `/app` the main cli tool.
- `/examples`s example Dhall and YAML configuration files for dhrun.
- `/src` dhrun types and logic.
- `/tests` golden, unit, quickcheck tests.
