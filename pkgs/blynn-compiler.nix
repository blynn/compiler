{ stdenvNoCC, lib, mescc-tools-seed }:

stdenvNoCC.mkDerivation {
  name = "blynn-compiler";
  src = lib.cleanSource ../.;
  nativeBuildInputs = [ mescc-tools-seed ];

  postPatch = ''
    patchShebangs go.sh
  '';

  buildPhase = ''
    ./go.sh
  '';

  installPhase = ''
    mkdir -p $out/bin $out/share
    cp bin/raw $out/share
    cp bin/vm $out/bin
  '';
}
