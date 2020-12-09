# Bootstrappable packages as a Nixpkgs overlay
self: super:

{
  blynn-compiler = super.callPackage ./blynn-compiler.nix { };
  kaem = super.callPackage ./kaem.nix { };
  m2-planet = super.callPackage ./m2-planet.nix { };
  mes-m2 = super.callPackage ./mes-m2.nix { };
  mescc-tools = super.callPackage ./mescc-tools.nix { };
  mescc-tools-seed = super.callPackage ./mescc-tools-seed.nix { };
}
