{
  pkgs ? import ./nix {},
}:
rec {
  dhrun = pkgs.dhrun;
  hack = pkgs.lib.getHackEnv pkgs.bleeding pkgs pkgs.bleedingHaskellPackages dhrun;
}
