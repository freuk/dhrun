{ # host package set (unused aside for fetching nixpkgs)
  hostPkgs         ? import <nixpkgs> {}
, # versioned nixpkgs
  pkgs             ? import (hostPkgs.nix-update-source.fetch ./pkgs.json).src {}
, # versioned nixpkgs-older version for zmcat
  bleeding         ? import (hostPkgs.nix-update-source.fetch ./bleeding.json).src {}
, # fetcher alias for the remaining arguments
  fetched          ? s: (pkgs.nix-update-source.fetch s).src
  # source path for dhrun
, dhrun-src        ? ../.
}:
let
  panhandle-src = fetched panhandle/pin.json;

  callPackage = pkgs.lib.callPackageWith (pkgs // dhrunpkgs);

  dhrunpkgs = rec {
    inherit bleeding;
    lib       = import ./utils.nix;
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: with pkgs.haskell.lib;
      rec {
        panpipe = doJailbreak super.panpipe;
        panhandle = (self.callCabal2nix "panhandle" (builtins.fetchGit {
          inherit (panhandle-src) url rev; })) {};
      };
    };
    bleedingHaskellPackages = bleeding.haskellPackages.override {
      overrides = self: super: with bleeding.haskell.lib;
      rec {
        dhrun = (self.callCabal2nix "dhrun" (lib.filter dhrun-src) ) {};
      };
    };
    dhrun = bleedingHaskellPackages.dhrun.overrideAttrs (old:{
        installPhase = old.installPhase + ''
          mkdir -p $out/share/
          cp -r resources $out/share/
        '';
      });
  };
in dhrunpkgs
