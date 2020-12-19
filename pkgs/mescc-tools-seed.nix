{ stdenv, fetchFromGitHub, git }:

stdenv.mkDerivation {
  pname = "mescc-tools-seed";
  version = "unstable-2020-12-16";

  nativeBuildInputs = [ git ];
  src = fetchFromGitHub {
    owner = "oriansj";
    repo = "mescc-tools-seed";
    rev = "36402ef134e0ae04b7d752b6f4dd262d2de583d1";
    sha256 = "0lcg4dq2zhpzm8m90cbsaqhh63q6prm1hvsxap4r6q6kxmxj66nf";
    fetchSubmodules = true;
  };
  makeFlags = [ "PREFIX=${placeholder "out"}" ];
  installPhase = ''
    mkdir -p $out/bin
    cp ./bin/* $out/bin
  '';
}
