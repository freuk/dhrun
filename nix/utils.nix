let
  f = { mkDerivation, stdenv
      , cabal-install
      , apply-refact
      , hdevtools
      , Glob
      , hindent
      , fswatch
      , hlint
      , protolude
      , shake
      , Cabal
      , ghcid
      , typed-process
      , unix
      , cabal-helper
      }:
      mkDerivation {
        pname = "dummy";
        version = "";
        src = "";
        libraryHaskellDepends = [
            cabal-install
            #apply-refact
            #hdevtools
            #hindent
            #fswatch
            #hlint
            #protolude
            shake
            Cabal
            #Glob
            ghcid
            #typed-process
            #unix
        ];
        description = "";
        license = stdenv.lib.licenses.mit;
      };

in
rec {
  filter = path:
    builtins.filterSource (path: _:
    (baseNameOf path != ".hdevtools.sock") &&
    (baseNameOf path != ".ghc.environment.x86_64-linux-8.4.4") &&
    (baseNameOf path != "_output") &&
    (baseNameOf path != "result") &&
    (baseNameOf path != "README") &&
    (baseNameOf path != ".README") &&
    (baseNameOf path != "dist")) path;

  devInputs = p: [
    p.git
    p.hwloc
    p.htop
    p.jq
  ];

  getHackEnv = pkgs: util-pkgs: super: package : super.shellFor {
    packages = p: [
      package
      (pkgs.haskellPackages.callPackage f {})
    ];
    withHoogle = true;
    buildInputs = devInputs pkgs ++ package.buildInputs ++(with util-pkgs.haskellPackages; [brittany panpipe panhandle]);
  };
}
