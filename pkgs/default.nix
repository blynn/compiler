{ sources ? import ../nix/sources.nix, pkgs ? import sources.nix }:

import sources.nixpkgs {
  overlays = [ (import ./packages.nix) ];
}
