{ sources ? import ./nix/sources.nix { }
, pkgs ? import sources.nixpkgs { }
}:
let
  # Nixpkgs extended with bootstrappable related packages
  bootstrappable-pkgs = import ./pkgs { };
in
bootstrappable-pkgs
