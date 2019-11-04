{ hostPkgs ? import <nixpkgs> { }

, pkgs ? import (hostPkgs.nix-update-source.fetch ./pkgs.json).src { }

, fetched ? s: (pkgs.nix-update-source.fetch s).src

, dhrun-src ? ../.

}:

let
  panhandle-src = fetched panhandle/pin.json;

  callPackage = pkgs.lib.callPackageWith (pkgs // dhrunpkgs);

  cabalFile = dhallSpec:
    pkgs.runCommand "cabalFile" { } ''
      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8
      cp ${dhallSpec} cabal.dhall
      substituteInPlace dhrun.dhall --replace "= ./nix/dhall2cabal" "= ${./dhall2cabal}"
      GHCVERSION=$(${pkgs.haskellPackages.ghc}/bin/ghc --numeric-version)
      ${pkgs.haskellPackages.dhall-to-cabal}/bin/dhall-to-cabal <<< "./cabal.dhall" --output-stdout > $out
    '';

  patchedSrc = source: dhallFile:
    pkgs.runCommand "patchedSrc" { } ''
      mkdir -p $out
      cp -r ${source}/ $out
      chmod -R +rw $out
      cp ${cabalFile dhallFile} $out/hsnrm.cabal
    '';

  hack = pkgs.haskellPackages.shellFor {
    packages = p: [
      dhrunpkgs.dhrun
      (pkgs.haskellPackages.callPackage ./hs-tools { })
    ];
    withHoogle = true;
    buildInputs = [ pkgs.git pkgs.hwloc pkgs.htop pkgs.jq ];
    CABALFILE = cabalFile ../dhrun.dhall;
  };

  dhrunpkgs = rec {
    inherit pkgs;
    lib = import ./utils.nix;
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super:
        with pkgs.haskell.lib; rec {
          panpipe = doJailbreak super.panpipe;
          panhandle = (self.callCabal2nix "panhandle"
            (builtins.fetchGit { inherit (panhandle-src) url rev; })) { };
        };
    };
    pkgsHaskellPackages = pkgs.haskellPackages.override {
      overrides = self: super:
        with pkgs.haskell.lib; rec {
          dhrun = (self.callCabal2nix "dhrun"
            (patchedSrc (lib.filter dhrun-src) ../dhrun.dhall)) { };
        };
    };
    dhrun = pkgsHaskellPackages.dhrun.overrideAttrs (old: {
      installPhase = old.installPhase + ''
        mkdir -p $out/share/
        cp -r resources $out/share/
      '';
    });
  };
in pkgs // {
  inherit dhrunpkgs;
  inherit hack;
}
