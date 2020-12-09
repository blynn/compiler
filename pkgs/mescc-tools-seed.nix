{ stdenvNoCC, fetchFromGitHub }:

stdenvNoCC.mkDerivation rec {
  name = "mescc-tools-seed";

  src = fetchFromGitHub {
    owner = "oriansj";
    repo = name;
    rev = "58e9a4249cde3faead999f94dea5f64c031bb76a";
    sha256 = "0xib57ygdck8zskhaf4y0msgj24w3xk3slqz4dcfg25pcgg6ymvg";
    fetchSubmodules = true;
  };

  makeFlags = [ "PREFIX=${placeholder "out"}" ];
  installPhase = ''
    mkdir -p $out/bin
    cp ./bin/* $out/bin
  '';
}
