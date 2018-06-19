{nixpkgs ? import <nixpkgs> {} }:
self: super: {
  validation = super.callHackage "validation" "0.6.0" {}; #fix doctest error
  perConstructor-sop = super.callCabal2nix "perConstructor-sop" (nixpkgs.pkgs.fetchFromGitHub {
    owner = "adamConnerSax";
    repo = "perConstructor-sop";
    rev = "b0c8fd5c4b1576b4ad713821f35d06b0c00ff5f6";
    sha256 = "1lyzzn1bfjgk6p8v62r5r0xqkp6ry4y26f52d3zfix7n1xqfqaq4";
   }) {};
}
