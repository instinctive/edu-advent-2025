let
  pkgs = import <nixpkgs> {};
  haskellEnv = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
        cabal-install
        ghcid
        markdown-unlit
      ]);
    returnShellEnv = true;
  };
in
  haskellEnv.overrideAttrs (oldAttrs: {
    shellHook = (oldAttrs.shellHook or "") + ''
      getInput () {
          curl https://adventofcode.com/$(basename `pwd`)/day/$1/input -H "`cat cookie`" > i/$1
      }
    '';
  })
