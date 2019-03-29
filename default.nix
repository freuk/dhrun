{
  pkgs ? import <argopkgs> {dhall-exec-src=./.;},
}:
rec {
  dhall-exec = pkgs.dhall-exec;
  hack = pkgs.argolib.getHackEnv pkgs pkgs.haskellPackages dhall-exec;
}
