(import ./reflex-platform {}).project ({ pkgs, ... }:
let nixpkgs = import <nixpkgs> {};
in
{        
  packages = {
    reflex-sumType-utilities = ./. ; 
  };

  shells = {
   ghc = ["reflex-sumType-utilities"];
   ghc8_2_1 = ["reflex-sumType-utilities"];
  };
  
  overrides = import ./package-overlay.nix {};

  withHoogle = false;
})
