{
  pkgs ? import <argopkgs> {dhrun-src=./.;},
}:
rec {
  dhrun = pkgs.dhrun;
  dhrunt = pkgs.dhrun.overrideAttrs (_:{doCheck=true;});
  hack = pkgs.argolib.getHackEnv pkgs.bld pkgs.bld.haskellPackages dhrun;
}
