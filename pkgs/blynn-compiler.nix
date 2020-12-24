{ stdenvNoCC, lib, mescc-tools, m2-planet }:

stdenvNoCC.mkDerivation {
  name = "blynn-compiler";
  src = lib.cleanSource ../.;
  nativeBuildInputs = [ mescc-tools m2-planet ];

  postPatch = ''
    patchShebangs go.sh
    patchShebangs filter_haskell_output
  '';

  buildPhase = ''
    ./go.sh
  '';

  installPhase = ''
    mkdir -p $out/bin $out/share
    cp bin/raw $out/share
    cp bin/vm $out/bin
    cp bin/precisely $out/bin
  '';
}
