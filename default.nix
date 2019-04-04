{
  pkgs ? import <argopkgs> {dhrun-src=./.;},
}:
rec {
  dhrun = pkgs.dhrun;
  hack = pkgs.argolib.getHackEnv pkgs pkgs.haskellPackages dhrun;
}
