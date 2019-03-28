{
  pkgs ? import <argopkgs> {dhall-executor-src=./.;},
}:
rec {
  dhall-executor = pkgs.dhall-executor;
  hack = pkgs.argolib.getHackEnv pkgs pkgs.haskellPackages dhall-executor;
}
