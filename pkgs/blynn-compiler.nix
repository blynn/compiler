{ stdenv, lib, help2man, texinfo, mescc-tools, mes-m2, m2-planet }:

stdenv.mkDerivation {
  name = "blynn-compiler";
  src = lib.cleanSource ../.;
  nativeBuildInputs = [ help2man texinfo mescc-tools m2-planet mes-m2 ];

  postPatch = ''
    patchShebangs go.sh
  '';

  buildPhase = ''
    ./go.sh
  '';

  installPhase = ''
    mkdir -p $out/share
    cp bin/vm $out/share
  '';
}
